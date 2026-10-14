{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Saline.Test.Script where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, cancelMany)
import Control.Lens hiding (Level)
import Control.Monad (join, void)
import Control.Monad.Except (ExceptT, mapExceptT, runExceptT, throwError, catchError, withExceptT)
import Control.Monad.Free (Free, iterM, liftF)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (StateT, gets, modify', runStateT)
import Data.Aeson (FromJSON)
import Data.Foldable (toList, find, forM_)
import Data.IORef (newIORef, IORef)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Void (Void, absurd)
import Network.Tendermint.Client (ResultTx, ResultHealth, ResultABCIInfo)
import Obelisk.Route (R, pattern (:/))
import Saline.Crypto (BIP32Child, SecretKey, unsafeGenerateTestAccount, PublicKey, derivePublicKey, genUUID)
import Saline.Intents (Intent, Wallet)
import Saline.Intents.Condition (Amount, Token, Asset)
import Saline.Interface.Query (CometQuery(..), SalineQuery(..), Req(..), Trade)
import Saline.Network (withTestNetwork)
import Saline.Network.CometBFT.Config
import Saline.Network.CometBFT.CLI (runNetworkNode, addCometBFTNode, nthNodePorts)
import Saline.Network.CometBFT.Node
import Saline.Network.Node.RPC qualified as RPC
import Saline.Network.Node.Storage (Store (..))
import Saline.Network.Node.Transaction
import Saline.Test.Prometheus (Samples, parsePrometheus)
import Saline.Utils qualified as Utils
import Test.Tasty.HUnit (Assertion)
import Text.URI (URI)
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy as LT
import qualified Data.Text.IO as T
import qualified Network.Wreq as Wreq
import qualified Saline.Network.Node.Handlers as Handlers

-- * test script interface

data ScriptCfg = ScriptCfg
  { numNodes :: Word
  , genesis  :: Store
  , instrumented :: Bool -- whether to enable prometheus in tendermint nodes
  } deriving stock Show

data Level = Level_Handler | Level_Network

type family Degrade (l :: Level) (a :: Type) :: Type

type instance Degrade Level_Network a = a

type instance Degrade Level_Handler NetworkState = ()
type instance Degrade Level_Handler (Hash SignedTx) = ()
type instance Degrade Level_Handler ResultTx = ()
type instance Degrade Level_Handler URI = Void
-- What we really want is `Degrade Level_Handler (Maybe URI) = ()` but doing it this way allows for polymorphic Nothing

data ScriptF (l :: Level) r where
  NetworkF :: Network_ScriptF r -> ScriptF Level_Network r
  AgnosticF :: Agnostic_ScriptF r -> ScriptF l r
  DegradableF :: Degradable_ScriptF l r -> ScriptF l r
deriving stock instance Functor (ScriptF l)

data Agnostic_ScriptF r
  = NewWallet ((SecretKey, PublicKey) -> r)
  | GenNonce (Nonce -> r)
  | WithWatcherKeys (WatcherKeys -> r)
  | Sleep Word r
  | Assert Assertion r
  | Info T.Text [T.Text] r
  deriving stock Functor

data Degradable_ScriptF l r
  = SendTx (Maybe (Degrade l URI)) SignedTx (Degrade l (Hash SignedTx) -> r)
  | WaitTx (Maybe (Degrade l URI)) (Degrade l (Hash SignedTx)) (Degrade l ResultTx -> r)
  | forall a b. (FromJSON b, Show b, Show a) => SalineQ (Maybe (Degrade l URI)) (SalineQuery (Req a b)) a (b -> r)
deriving stock instance Functor (Degradable_ScriptF l)

data Network_ScriptF r
  = StartNode CometBFTNode r
  | StopNode CometBFTNode r
  | WaitNode CometBFTNode r
  | NewNode (CometBFTNode -> r)
  | HealthNode CometBFTNode (ResultHealth -> r)
  | ABCIInfoNode CometBFTNode (ResultABCIInfo -> r)
  | PrometheusNode CometBFTNode (Samples -> r)
  deriving stock Functor
  -- later: | NewNode (CometBFTNode -> r)
  --        | DelNode CometBFTNode r

-- TODO: add/delete node to the network (not just start/stop initial ones)
-- TODO: don't assume/enforce success of all interactions
-- TODO: (more) tendermint queries?

type Script l = Free (ScriptF l)

newWallet :: Script l (SecretKey, PublicKey)
newWallet = liftF $ AgnosticF $ NewWallet id

startNode, stopNode, waitForNode :: CometBFTNode -> Script Level_Network ()
(startNode, stopNode, waitForNode) = (impl StartNode, impl StopNode, impl WaitNode)
  where impl f n = liftF $ NetworkF $ f n ()

startNodes, stopNodes, waitForNodes :: Foldable f => f CometBFTNode -> Script Level_Network ()
startNodes = mapM_ startNode
stopNodes = mapM_ stopNode
waitForNodes = mapM_ waitForNode

newNode :: Script Level_Network CometBFTNode
newNode = liftF $ NetworkF (NewNode id)

nodeHealth :: CometBFTNode -> Script Level_Network ResultHealth
nodeHealth n = liftF $ NetworkF $ HealthNode n id

nodeABCIInfo :: CometBFTNode -> Script Level_Network ResultABCIInfo
nodeABCIInfo n = liftF $ NetworkF $ ABCIInfoNode n id

genNonce :: Script l Nonce
genNonce = liftF $ AgnosticF $ GenNonce id

sleep :: Word -> Script l ()
sleep n = liftF $ AgnosticF $ Sleep n ()

sendTx :: SignedTx -> Script l (Degrade l (Hash SignedTx))
sendTx stx = liftF $ DegradableF $ SendTx Nothing stx id

sendTxTo :: URI -> SignedTx -> Script Level_Network (Hash SignedTx)
sendTxTo u stx = liftF $ DegradableF $ SendTx (Just u) stx id

signTx :: SecretKey -> Tx -> Script l SignedTx
signTx sk tx = (\n -> mkSignedTxWithNonce n (tx, sk)) <$> genNonce

waitTx :: Degrade l (Hash SignedTx) -> Script l (Degrade l ResultTx)
waitTx htx = liftF $ DegradableF $ WaitTx Nothing htx id

waitTxTo :: Degrade l URI -> Degrade l (Hash SignedTx) -> Script l (Degrade l ResultTx)
waitTxTo u htx = liftF $ DegradableF $ WaitTx (Just u) htx id

watcherKeys :: Script l WatcherKeys
watcherKeys = liftF $ AgnosticF $ WithWatcherKeys id

onboard :: PublicKey -> Amount -> Token -> Script l (Degrade l (Hash SignedTx))
onboard account n tok = do
  (watcherSK, watcherPK) <- watcherKeys
  let tx = simpleTx . pure $
        Instruction_Bridge account (BridgeInstruction_Mint watcherPK tok n)
  stx <- signTx watcherSK tx
  sendTx stx

offboard :: (SecretKey, PublicKey) -> Amount -> Token -> Script l (Degrade l (Hash SignedTx))
offboard (accountSK, accountPK) n tok = do
  let tx = simpleTx . pure $
        Instruction_Bridge accountPK (BridgeInstruction_Burn tok n)
  stx <- signTx accountSK tx
  sendTx stx

instructions :: NonEmpty Instruction -> NonEmpty SecretKey -> Script l (Degrade l (Hash SignedTx))
instructions instrs (sk :| sks) = sendTx . addSignatures sks =<< signTx sk (simpleTx instrs)

transfer
  :: PublicKey -> PublicKey -> Asset Map -> NonEmpty SecretKey
  -> Script l (Degrade l (Hash SignedTx))
transfer a b what signers = instructions (pure $ Instruction_Funds a b what) signers

swap :: PublicKey -> Asset Map -> PublicKey -> Asset Map -> NonEmpty SecretKey -> Script l (Degrade l (Hash SignedTx))
swap a whatA b whatB signers =
  instructions
    (NonEmpty.fromList
      [ Instruction_Funds a b whatA
      , Instruction_Funds b a whatB
      ]
    )
    signers

setIntent :: (SecretKey, PublicKey) -> Intent -> Script l (Degrade l ((Hash SignedTx)))
setIntent (sk, a) int = instructions (pure $ Instruction_SetIntent a int) (pure sk)

addIntent :: (SecretKey, PublicKey) -> Intent -> Script l (Degrade l ((Hash SignedTx)))
addIntent (sk, a) int = instructions (pure $ Instruction_OrIntent a int) (pure sk)

clearIntent :: (SecretKey, PublicKey) -> Script l (Degrade l ((Hash SignedTx)))
clearIntent (sk, a) = instructions (pure $ Instruction_Delete a) (pure sk)

query :: (FromJSON b, Show b, Show a) => SalineQuery (Req a b) -> a -> Script l b
query sq a = liftF $ DegradableF $ SalineQ Nothing sq a id

queryTo :: (FromJSON b, Show b, Show a) => URI -> SalineQuery (Req a b) -> a -> Script Level_Network b
queryTo u sq a = liftF $ DegradableF $ SalineQ (Just u) sq a id

getIntent :: PublicKey -> Script l (Maybe Intent)
getIntent pk = query SalineQuery_Intent pk

getIntents :: Script l (Map Intent (Map PublicKey ()))
getIntents = query SalineQuery_Intents ()

getBalance :: PublicKey -> Token -> Script l Amount
getBalance = curry $ query SalineQuery_Balance

getBalances :: PublicKey -> Script l (Asset Map)
getBalances = query SalineQuery_Aggregate . Set.singleton

getWallet :: PublicKey -> Script l Wallet
getWallet pk = query SalineQuery_Wallet pk

getAggregate :: Set PublicKey -> Script l (Map Token Amount)
getAggregate pks = query SalineQuery_Aggregate pks

getSwaps :: PublicKey -> (Token, Token) -> Script l (Map (Trade Amount) (NonEmpty Tx))
getSwaps pk toks = query SalineQuery_Swaps (pk, toks)

getPrometheusMetrics :: CometBFTNode -> Script Level_Network Samples
getPrometheusMetrics n = liftF $ NetworkF $ PrometheusNode n id

checks :: Assertion -> Script l ()
checks assertion = liftF $ AgnosticF $ Assert assertion ()

info :: T.Text -> [T.Text] -> Script l ()
info lbl xs = liftF $ AgnosticF $ Info lbl xs ()

defCfg :: ScriptCfg
defCfg = ScriptCfg
  { numNodes = 2
  , genesis = defGenesis
  , instrumented = False
  }

defGenesis :: Store
defGenesis = Store
  { _store_balances  = mempty
  , _store_intents   = mempty
  , _store_oracles   = mempty
  , _store_contracts = mempty
  , _store_timestamp = 0
  }

-- * implementation

type ScriptM l = ExceptT T.Text (StateT (ScriptState l) IO)

type WatcherKeys = (SecretKey, PublicKey)

type AppNet = AppNetwork (R CometQuery) Store

data ScriptState l = ScriptState
  { _nextWallet :: BIP32Child
  , _watcher    :: WatcherKeys
  , _network    :: Degrade l NetworkState
  }
data NetworkState = NetworkState
  { _nodes      :: Map CometBFTNode (Maybe (Async (), Async ()))
  , _net        :: AppNet
  , _netRootDir :: T.Text
  }

fmap join $ sequence
  [ makeLenses ''NetworkState
  , makeLenses ''ScriptState
  ]

defScriptState :: WatcherKeys -> Degrade l NetworkState -> ScriptState l
defScriptState ks n = ScriptState
  { _nextWallet = 0
  , _watcher    = ks
  , _network    = n
  }

defNetworkState :: AppNet -> NonEmpty CometBFTNode -> T.Text -> NetworkState
defNetworkState appnet ns rootDir = NetworkState
  { _nodes      = Map.fromList $ map ((,Nothing)) (toList ns)
  , _net        = appnet
  , _netRootDir = rootDir
  }

runScript :: (SecretKey -> IO (Either T.Text a)) -> IO a
runScript runner = runner watcherAccount >>= \case
  Left err -> error ("Script error: " ++ T.unpack err)
  Right a -> return a

-- Public deterministic watcher fixture; never use on a real network.
watcherAccount :: SecretKey
watcherAccount = unsafeGenerateTestAccount 0
  "edit local depend must evidence fabric safe excess vintage find option soccer"

handlerLevel :: ScriptCfg -> Script Level_Handler a -> SecretKey -> IO (Either T.Text a)
handlerLevel ScriptCfg{..} script watcherSK = do
  store <- newIORef genesis

  let
    watcherPK = derivePublicKey watcherSK
    state0 = defScriptState @Level_Handler (watcherSK, watcherPK) ()
    go = flip iterM script $ \case
      AgnosticF f -> agnosticStep f
      DegradableF f -> degradableStepHandler store f

  fmap fst $ runStateT (runExceptT go) state0

networkLevel :: ScriptCfg -> (NonEmpty CometBFTNode -> Script Level_Network a) -> SecretKey -> IO (Either T.Text a)
networkLevel ScriptCfg{..} script watcherSK = do
  let watcherPK = derivePublicKey watcherSK
  withTestNetwork genesis numNodes instrumented watcherPK $ \appnet rootDir ns ->
    case NonEmpty.nonEmpty ns of
      Nothing -> return $ Left "0 node in the network?!"
      Just ns' -> do
        let state0 = defScriptState @Level_Network (watcherSK, watcherPK) $ defNetworkState appnet ns' rootDir
            go     = do
              res <- flip iterM (script ns') $ \case
                AgnosticF s -> agnosticStep s
                DegradableF f -> degradableStepNetwork f
                NetworkF f -> networkStep f
              ns'' <- gets $ _nodes . _network
              liftIO (cleanup ns'')
              return res
        runStateT (runExceptT go) state0 >>= \case
          (Left e, st) -> cleanup (_nodes $ _network st) >> return (Left e)
          (Right a, _) -> return (Right a)

  where
    cleanup ns = forM_ ns $ \case
      Just (a, b) -> liftIO $ cancelMany [a, b]
      Nothing     -> return ()

newWalletM :: ScriptM l (SecretKey, PublicKey)
newWalletM = do
  i <- gets (view nextWallet)
  nextWallet += 1
  return $ mnemonics !! fromEnum i
  where
    -- Public fixture: every derived test wallet is reproducible from this phrase.
    mnemonic0 = "machine magic speed trouble cargo patrol oven embody remain top fetch wool"
    mnemonics = flip fmap [0..] $ \i ->
      let sk = unsafeGenerateTestAccount i mnemonic0
      in (sk, derivePublicKey sk)

getURI :: Maybe URI -> ScriptM Level_Network URI
getURI muri = case muri of
  Just u -> return u
  Nothing -> do
    mnode <- find (isJust . snd) . Map.toList <$> gets (_nodes . _network)
    case mnode of
      Just (node, Nothing) -> throwError $ "sendTx: can't send tx to non running node " <> Utils.tshow node
      Just (node, Just _) -> return (getNodeURI node)
      Nothing -> throwError "sendTx: no running node, can't send tx"

sendTxM :: Maybe URI -> SignedTx -> ScriptM Level_Network (Hash SignedTx)
sendTxM muri stx = do
  u <- getURI muri
  r <- liftIO (RPC.broadcastTxSync stx u)
  case r of
    Left err -> throwError $ sendTxError stx err
    Right h  -> return h

sendTxError :: SignedTx -> T.Text -> T.Text
sendTxError stx err = T.unlines ["sendTx error:", "tx = " <> Utils.tshow stx, "error = " <> err]

waitTxM :: Maybe URI -> Hash SignedTx -> ScriptM Level_Network ResultTx
waitTxM muri htx = do
  u <- getURI muri
  r <- liftIO (go u n)
  case r of
    Left err -> throwError $ T.unlines ["waitTx error:", "tx = " <> Utils.tshow htx, "error = " <> err]
    Right resultTx -> return resultTx

  where n = 50
        go :: URI -> Word -> IO (Either T.Text ResultTx)
        go _ 0 = return $ Left ("stopping after " <> Utils.tshow n <> " attempts")
        go u k = RPC.transaction htx u
               >>= either (\_ -> threadDelay 200_000 >> go u (k-1))
                          (return . Right)

startNodeM :: CometBFTNode -> ScriptM Level_Network ()
startNodeM n = zoom network $ do
  appnet <- gets _net
  ns <- gets _nodes
  case Map.lookup n ns of
    Nothing -> throwError $ "startNode error: unknown node " <> Utils.tshow n
    Just Nothing -> do
      asyncs <- runNetworkNode appnet n
      modify' $ \s -> s { _nodes = Map.insert n (Just asyncs) (_nodes s) }
    Just (Just _) -> throwError $ "startNode error: node " <> Utils.tshow n <> " is already running"

stopNodeM :: CometBFTNode -> ScriptM Level_Network ()
stopNodeM n = zoom network $ do
  ns <- gets _nodes
  case Map.lookup n ns of
    Nothing -> throwError $ "stopNode error: unknown node " <> Utils.tshow n
    Just Nothing -> throwError $ "stopNode error: node " <> Utils.tshow n <> " is not running"
    Just (Just (comet, saline)) -> do
      liftIO $ cancelMany [comet, saline]
      modify' $ \s -> s { _nodes = Map.insert n Nothing (_nodes s) }

waitNodeM :: CometBFTNode -> ScriptM Level_Network ()
waitNodeM node = void (go n)
  where u = getNodeURI node
        n = 20 :: Word
        go 0 = throwError ("waitNodeM error\ndoesn't answer after " <> Utils.tshow n <> " attempts")
        go k = do
          queryM (Just u) SalineQuery_Intents () -- we query the intents because no input required, really just checks if it's up
            `catchError` \_ -> liftIO (threadDelay 200_000) >> go (k-1)

newNodeM :: ScriptM Level_Network CometBFTNode
newNodeM = do
  newNodeIdx <- use (network.net.appNetwork_size)
  let moniker = "node" <> Utils.tshow newNodeIdx
      ports = nthNodePorts newNodeIdx
  nodedir <- (\d -> d <> "/" <> moniker) <$> use (network.netRootDir)
  refNode <- use (network.nodes.to Map.keys) >>= \ns -> case listToMaybe ns of
    Just n -> return n
    Nothing -> throwError "newNodeM error\nno existing node to imitate?"
  n <- liftIO $ addCometBFTNode nodedir moniker ports refNode
  network.nodes %= Map.insert n Nothing
  network.net.appNetwork_size += 1
  return n

queryM :: (Show a, FromJSON b, Show b) => Maybe URI -> SalineQuery (Req a b) -> a -> ScriptM Level_Network b
queryM muri q inputs = do
  se <- zoom network $ gets (_appNetwork_queryEncoder . _net)
  u <- getURI muri
  liftIO (RPC.query se (CometQuery_Store :/) q inputs u) >>= \case
    Left e -> throwError $ queryError q inputs e
    Right b -> return b

queryError :: (Show a, FromJSON b, Show b) => SalineQuery (Req a b) -> a -> T.Text -> T.Text
queryError q inputs e = T.unlines
  [ "queryM error"
  , "on q=" <> Utils.tshow q <> ", inputs=" <> Utils.tshow inputs
  , "error: " <> e
  ]

healthM :: CometBFTNode -> ScriptM l ResultHealth
healthM n = liftIO (RPC.health $ getNodeURI n) >>= \case
  Left e -> throwError $
    T.unlines [ "health check error against node " <> Utils.tshow n
              , "error: " <> e
              ]
  Right a -> pure a

abciInfoM :: CometBFTNode -> ScriptM l ResultABCIInfo
abciInfoM n = liftIO (RPC.abciInfo $ getNodeURI n) >>= \case
  Left e -> throwError $
    T.unlines [ "ABCI info error against node " <> Utils.tshow n
              , "error: " <> e
              ]
  Right a -> pure a

prometheusM :: CometBFTNode -> ScriptM l Samples
prometheusM n = liftIO (Wreq.get $ T.unpack ("http://localhost" <> promLAddr)) >>= \r ->
  case parsePrometheus (r ^. Wreq.responseBody . to (LT.toStrict . LT.decodeUtf8)) of
    Left e -> throwError $
      T.unlines [ "couldn't parse prometheus metrics for node " <> Utils.tshow n
                , "error: " <> T.pack e
                ]
    Right a -> pure a
  where
    promLAddr = n ^. cometBFTNode_config.config_instrumentation.configInstr_prometheus_listen_addr

infoM :: T.Text -> [T.Text] -> ScriptM l ()
infoM lbl = liftIO . T.putStrLn . T.unlines . (("info> " <> lbl):) . map ("λ> " <>)

agnosticStep :: Agnostic_ScriptF (ScriptM l a) -> ScriptM l a
agnosticStep = \case
  NewWallet k -> newWalletM >>= k
  GenNonce k -> liftIO genUUID >>= k
  WithWatcherKeys k -> gets _watcher >>= k
  Sleep n k -> liftIO (Utils.sleep n) >> k
  Assert assertion k -> liftIO assertion >> k
  Info lbl lns k -> infoM lbl lns >> k

networkStep :: Network_ScriptF (ScriptM Level_Network a) -> ScriptM Level_Network a
networkStep = \case
  StartNode n k -> startNodeM n >> k
  StopNode n k -> stopNodeM n >> k
  WaitNode n k -> waitNodeM n >> k
  NewNode k -> newNodeM >>= k
  HealthNode n k -> healthM n >>= k
  ABCIInfoNode n k -> abciInfoM n >>= k
  PrometheusNode n k -> prometheusM n >>= k

degradableStepNetwork :: Degradable_ScriptF Level_Network (ScriptM Level_Network a) -> ScriptM Level_Network a
degradableStepNetwork = \case
  SalineQ muri q inputs k -> queryM muri q inputs >>= k
  SendTx muri stx k -> sendTxM muri stx >>= k
  WaitTx muri htx k -> waitTxM muri htx >>= k

degradableStepHandler :: IORef Store -> Degradable_ScriptF Level_Handler (ScriptM Level_Handler a) -> ScriptM Level_Handler a
degradableStepHandler store = \case
  SalineQ u q inputs k -> (>>= k) $ assertUnique u $
    liftIO $ Handlers.query' store q inputs

  SendTx u stx k -> (>>= k) $ assertUnique u $ do
    (_, pk) <- gets _watcher
    withExceptT (sendTxError stx . prettyPrintTxError) $ mapExceptT liftIO $ Handlers.deliver' store pk stx

  WaitTx u _ k -> assertUnique u $ k ()
  where
    assertUnique :: Maybe Void -> x -> x
    assertUnique = maybe id absurd
