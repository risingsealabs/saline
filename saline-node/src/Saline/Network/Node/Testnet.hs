{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
module Saline.Network.Node.Testnet where

import Data.Dependent.Map qualified as DMap
import Data.Foldable
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Map (Map)
import Data.Map qualified as Map

import Saline.Crypto
import Saline.Intents.Condition
import Saline.Intents.Instrument hiding (faucet)
import Saline.Intents.Instrument qualified as Instrument
import Saline.Network.Node.Storage
import Saline.Network.Node.Transaction
import Saline.Utils

(=:) :: k -> a -> Map k a
(=:) = Map.singleton

-- Public deterministic faucet fixture for the example testnet only.
-- Its private key is publicly reproducible; never fund it with real assets.
faucetAccount :: SecretKey
faucetAccount = unsafeGenerateTestAccount 0
  "rotate sibling rather resemble inmate boost tortoise true inquiry primary jewel breeze"

faucetTx :: PublicKey -> Tx
faucetTx beneficiary = simpleTx $ pure $ Instruction_Funds (derivePublicKey faucetAccount) beneficiary faucetTap

genesis :: Timestamp -> Store
genesis currentTime =
  let
    pk k v = derivePublicKey k =: v

    swapSymbol a b = tshow a <> tshow b

    betAmount = 1e3

    intents = fold
      [ pk faucetAccount $ snd Instrument.faucet
      , pk demoMnemonicA_1 $ snd forward
      , pk demoMnemonicA_2 $ snd multisig
      , pk demoMnemonicA_3 $ snd delegate
      , pk demoMnemonicA_4 $ snd dusting
      , pk demoMnemonicA_7 $ balanced
      ]

    contracts = Map.unionsWith Map.union
      [ opt 1 demoMnemonicZ_1 demoMnemonicB_5 (BTC  =: Expr_Lit 1)   (ETH  =: Expr_Lit 10)
      , opt 2 demoMnemonicZ_1 demoMnemonicB_5 (USDC =: Expr_Lit 1e3) (USDT =: Expr_Lit 1e3)
      , opt 3 demoMnemonicZ_2 demoMnemonicB_5 (USDC =: Expr_Lit 1e3) (USDT =: Expr_Lit 1e3)
      , opt 4 demoMnemonicZ_2 demoMnemonicB_5 (BTC  =: Expr_Lit 1)   (ETH  =: Expr_Lit 10)
      ]
      where
        opt n issuer holder recv send = Map.singleton i $ uncurryMap $ pk issuer $ pk holder n
          where i = swapPolicy Nothing recv send

    oracles = pk demoMnemonicA_6 $ DMap.singleton VariableTag_Amount $ Compose $ fold
      [ quote ETH  2e1
      , quote USDC 6e4
      , quote USDT 6e4
      ] where quote b v = swapSymbol BTC b =: Identity v

    balances = fold
      [ pk demoMnemonicA_1   $ USDC =: 2e5
      , pk demoMnemonicB_1   $ BTC  =:  10

      , pk demoMnemonicA_2   $ BTC  =:  10

      , pk demoMnemonicA_3   $ USDC =: 1e5
      , pk demoMnemonicB_3   $ mempty

      , pk demoMnemonicA_4   $ mempty
      , pk demoMnemonicB_4   $ SALT =: 1e2

      , pk demoMnemonicA_5   $ BTC =: 10
      , pk demoMnemonicB_5   $ USDC =: 150e3
      , pk demoMnemonicZ_1   $ USDT =: 1e4 <> ETH =: 100

      , pk demoMnemonicA_6   $ USDC  =: (2 * betAmount)
      , pk demoMnemonicB_6   $ BTC  =: 5
      ]

  in
    Store
      { _store_balances  = balances
      , _store_intents   = intents
      , _store_oracles   = oracles
      , _store_contracts = contracts
      , _store_timestamp = currentTime
      }
