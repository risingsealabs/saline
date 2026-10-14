{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
module Saline.Network where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync, Async, cancelMany)
import Control.Exception (finally)
import Control.Lens ((<>~))
import Control.Monad (forever, when)
import Data.Bool
import Data.Foldable
import Data.Function (fix, on, (&))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Time.Clock.POSIX
import Obelisk.Route (R, checkEncoder)
import Reflex.Dom.Core ((=:))
import System.IO.Temp (withSystemTempDirectory)
import Text.URI (URI, render)

import Saline.Crypto
import Saline.Intents.Condition
import Saline.Intents.Instrument qualified as Instrument
import Saline.Interface.Query
import Saline.Network.CometBFT.CLI
import Saline.Network.CometBFT.Node
import Saline.Network.Node.Handlers
import Saline.Network.Node.RPC
import Saline.Network.Node.Storage
import Saline.Network.Node.Testnet qualified as Testnet
import Saline.Network.Node.Transaction
import Saline.Utils (initProcess, sleep, tshow)

salineNetwork :: SalineEncoder -> Word -> Store -> Bool -> AppNetwork (R CometQuery) Store
salineNetwork se size genesis withInstrumentation = AppNetwork
  { _appNetwork_nodeEnvironment = NodeEnvironment_Thread
  , _appNetwork_withNode = runABCIDev
  , _appNetwork_size = size
  , _appNetwork_queryEncoder = se
  , _appNetwork_instrumentation = withInstrumentation
  , _appNetwork_genesis = genesis
  }

withSalineNetwork
  :: SalineEncoder
  -> Store
  -> Word
  -> Bool
  -> ((Text -> IO ()) -> IO ())
  -> (AppNetwork (R CometQuery) Store -> (SignedTx -> URI -> IO (Either Text (Hash SignedTx))) -> Text -> [CometBFTNode] -> IO ())
  -> IO ()
withSalineNetwork se genesis size withInstrumentation withDir f = withNetwork net withDir $ f net broadcastTxSync
  where net = salineNetwork se size genesis withInstrumentation

salineNetworkTest
  :: SalineEncoder -> Word -> Bool -> Store -> TrustedWatcher
  -> AppNetwork (R CometQuery) Store
salineNetworkTest se size withInstrumentation genesis watcher = AppNetwork
  { _appNetwork_nodeEnvironment = NodeEnvironment_Thread
  , _appNetwork_withNode = runABCITest watcher
  , _appNetwork_size = size
  , _appNetwork_queryEncoder = se
  , _appNetwork_instrumentation = withInstrumentation
  , _appNetwork_genesis = genesis
  }

withTestNetwork
  :: Store -> Word -> Bool -> TrustedWatcher
  -> (AppNetwork (R CometQuery) Store -> Text -> [CometBFTNode] -> IO a)
  -> IO a
withTestNetwork genesis size withInstrumentation watcher f = case checkEncoder salineQueryEncoder of
  Left err -> error ("withTestNetwork: bad encoder - " ++ show err)
  Right se -> withNetwork (net se) withTemp (f $ net se)
  where net se = salineNetworkTest se size withInstrumentation genesis watcher
        withTemp fun = withSystemTempDirectory "saline-node-tmp" (fun . Text.pack)

waitForBlock :: Height -> URI -> IO ()
waitForBlock n uri = fix $ \go -> do
  block (Just n) uri >>= \case
    Left _ -> threadDelay 1e6 *> go
    Right _ -> pure ()

withDevnet :: Bool -> Bool -> IO () -> IO ()
withDevnet persistent eternal run = case checkEncoder salineQueryEncoder of
    Left err -> error $ Text.unpack $ "withDevnet: " <> err
    Right se -> withAsync run (devnet se persistent eternal)

devnet :: SalineEncoder -> Bool -> Bool -> Async () -> IO ()
devnet se persistent eternal _async = do
  posixTime <- fmap realToFrac getPOSIXTime

  let
    -- Public deterministic fixtures for disposable local networks only.
    -- Never fund these accounts or reuse their phrases for real wallets.
    gen = unsafeGenerateTestAccount

    testAccount0      = gen 0 "machine magic speed trouble cargo patrol oven embody remain top fetch wool"
    testAccount1      = gen 1 "machine magic speed trouble cargo patrol oven embody remain top fetch wool"
    disabledAccount   = gen 2 "machine magic speed trouble cargo patrol oven embody remain top fetch wool"
    noIntentAccount   = gen 3 "machine magic speed trouble cargo patrol oven embody remain top fetch wool"

    swapHostAccountA  = gen 0 "torch giggle genre exile step east village jelly upset cook inside song"
    swapHostAccountB  = gen 1 "torch giggle genre exile step east village jelly upset cook inside song"
    swapHostAccountC  = gen 2 "torch giggle genre exile step east village jelly upset cook inside song"
    swapHostAccountD  = gen 3 "torch giggle genre exile step east village jelly upset cook inside song"
    restrictedAccount = gen 4 "torch giggle genre exile step east village jelly upset cook inside song"
    limitedAccount    = gen 5 "torch giggle genre exile step east village jelly upset cook inside song"

    genesis :: Store
    genesis = Testnet.genesis posixTime
      & store_balances <>~ balances
      & store_intents <>~ devnetIntents

      where
        pk k v = derivePublicKey k =: v

        balances = fold
          [ pk testAccount0      $ BTC  =: 10 <> ETH =: 20 <> SALT =: 1e2 <> USDC =: 1e5 <> USDT =: 1e5
          , pk testAccount1      $ BTC  =:  5 <> ETH =: 10 <> SALT =: 5e1 <> USDC =: 5e4 <> USDT =: 5e4

          , pk swapHostAccountA  $ USDT =: 10000
          , pk swapHostAccountB  $ USDT =: 10000 <> USDC =: 10000
          , pk swapHostAccountC  $ ETH  =: 500
          , pk swapHostAccountD  $ ETH  =: 450
          , pk restrictedAccount $ mempty
          ]

        devnetIntents = fold
          [ pk testAccount0     $ simplePolicy testAccount0 testAccount1
          , pk testAccount1     $ simplePolicy testAccount1 testAccount0
          , pk disabledAccount  $ Condition_Any 1 []
          , pk swapHostAccountA $ limit 5 (15, False) $ swapPolicy
              Nothing
              (USDC =: Expr_Lit 1e3)
              (USDT =: Expr_Lit 1e3)
          , pk swapHostAccountB $ uses 5 $ snd $ Instrument.swapHybrid
          , pk swapHostAccountC $ duration 15 True $ swapPolicy
              (Just $ derivePublicKey testAccount0)
              (BTC  =: Expr_Lit 2)
              (ETH  =: Expr_Lit 50)
          , pk swapHostAccountD $ Condition_Any 1 [ simpleSwap 45, simpleSwap 55 ]
          , pk restrictedAccount $ Condition_Restriction $ Binary (Expr_Receiving ETH) Relation_GE $ Expr_Lit 2
          , pk limitedAccount $ limit 5 (45, False) $ Condition_Any 1
              [ uses     2        $ signedBy testAccount0
              , duration 30 False $ signedBy testAccount1
              ]
          ]
          where
            signedBy = Condition_Signature . derivePublicKey
            seconds = (+ posixTime)
            uses = Condition_Finite
            duration = Condition_Temporary . seconds
            limit a b = uses a . uncurry duration b

            simplePolicy me them = Condition_Any 1
              [ signedBy me
              , receivingFunds (Just $ derivePublicKey them) $ BTC =: Expr_Lit 1
              ]

            simpleSwap eth = swapPolicy
              Nothing
              (BTC  =: Expr_Lit 2)
              (ETH  =: Expr_Lit eth)

  initProcess

  -- TODO: use something more type-safe than []
  withSalineNetwork se genesis 2 False (bool withTempDir withCurrentDir persistent) $ \appn broadcast _root nodes -> case fmap getNodeURI nodes of
    [a,b] -> do
      asyncs <- traverse (runNetworkNode appn) nodes
      flip finally (cancelMany $ concat [ [comet, saline] | (comet, saline) <- asyncs ]) $ do
        let
          tx nonce node sk sks t = do
            stx <- maybe mkSignedTx (\n i -> pure $ mkSignedTxWithNonce n i) nonce (t, sk)
            broadcast (addSignatures sks stx) node >>= \case
              Left err -> putStrLn "ERROR!" >> print err
              Right h -> Text.putStrLn $ ">>> tx " <> tshow h <> " broadcasted to " <> render node

          --TODO: simplify counterparty-fulfillment code with automatic witness verification? maybe off-chain solving?

          send nonce node extraSigners from to = tx nonce node from extraSigners . simpleTx . pure . (Instruction_Funds `on` derivePublicKey) from to
          swap nonce node extraSigners with host = tx nonce node with extraSigners . simpleTx . pure . (Instruction_Funds `on` derivePublicKey) with host

        --TODO: use a deterministic seed instead
        let
          nonce0 = Just (read "ad37ce9e-3fc6-4a81-9383-8d5594b9e227")
          nonce1 = Just (read "3f071a92-df09-4f8e-a35c-513bd377c2d2")
          nonce2 = Just (read "92874d2f-b643-4277-aa11-34dbafddc007")
          nonce3 = Just (read "14107571-ab1e-4edb-8f3e-fb28e7fc3a23")
          nonce4 = Just (read "06fde6a5-bc2d-4c98-884b-c801083758e3")
          nonce5 = Just (read "09589f5c-10bd-44cd-acfe-7d31f87c12e6")

        sleep 2

        send nonce0 a [] testAccount0 testAccount1 $ BTC =: 1
        sleep 1
        send nonce1 a [] testAccount0 testAccount1 $ BTC =: 1

        waitForBlock 2 a

        -- TODO: use Network.Tendermint.Client.subscribe instead of sleep
        send nonce2 a [] testAccount0 restrictedAccount $ ETH =: 2
        send nonce3 a [] testAccount0 noIntentAccount $ BTC =: 1

        --TODO: use solveSwap
        swap nonce4 b [] testAccount0 swapHostAccountA $ USDC =: 1e3 <> USDT =: negate 1e3

        tx nonce5 a testAccount0 [] $ Testnet.faucetTx $ derivePublicKey testAccount0

        waitForBlock 3 a

        when eternal $ forever $ do
          Text.putStrLn "Listening for network activity..."
          sleep 60

    _ -> do
      putStrLn "Network.devnet: invalid pattern match"
