{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module Saline.Intents.Instrument where

import Data.Dependent.Map qualified as DMap
import Data.Dependent.Sum
import Data.Foldable
import Data.Functor.Const
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Reflex.Dom.Core ((=:))

import Saline.Crypto (SecretKey, derivePublicKey, unsafeGenerateTestAccount)
import Saline.Intents
import Saline.Intents.Condition

-- Sending/receiving from the perspective of the counterparty
sends, receives :: Maybe Address -> AssetF Expr Map -> Condition Expr Expr []
sends = sendingFunds
receives = receivingFunds

var :: VariableTag a -> Text -> Expr a
var tag name = Expr_Var $ Var tag name

par :: VariableTag a -> Text -> (VariableTag a -> Text -> b) -> b
par x y f = f x y

pars :: (forall var. (forall x. VariableTag x -> Text -> var) -> [var]) -> VariableNames
pars f = DMap.fromListWithKey (const (<>)) $ f $ \tag name -> tag :=> Const (Set.singleton name)

swap :: (VariableNames, Intent)
swap = (parameters, i)
  where
    i = Condition_All
      [ Nothing `sends`    Map.fromList [usdc]
      , Nothing `receives` Map.fromList [usdt]
      ]

    usdc    = (USDC, amountParameter var)
    usdt    = (USDT, amountParameter var)

    amountParameter  = par VariableTag_Amount  "amount"
    addressParameter = par VariableTag_Address "party1"

    parameters = pars $ \p ->
      [ amountParameter  p
      , addressParameter p
      ]

swapHybrid :: (VariableNames, Intent)
swapHybrid = (mempty, i)
  where
    i = swapPolicy
      Nothing
      (USDC =: Expr_Lit 1e3 <> USDT =: Expr_Lit 1e3)
      (ETH  =: Expr_Lit 1)

limited :: (VariableNames, Intent)
limited = (parameters, i)
  where
    swaps uses t = Condition_Finite uses $ Condition_All
      [ Nothing `sends`    (ETH =: Expr_Lit 1)
      , Nothing `receives` (t =: Expr_Lit 1000)
      ]

    i = Condition_Any 1
      [ swaps 2 USDC
      , swaps 2 USDT
      ]

    addressParameter = par VariableTag_Address "party1"

    parameters = pars $ \p ->
      [ addressParameter p
      ]

delegate, dusting, faucet, forward, multisig :: (VariableNames, Intent)

delegate = (mempty,) $ Condition_Finite 5 $ Condition_All
  [ Condition_Signature $ derivePublicKey demoMnemonicB_3
  , sendingFunds (Just $ derivePublicKey demoMnemonicC_3) $ USDC =: Expr_Lit 100
  ]

dusting = (mempty,) $ Condition_Restriction $ Binary (Expr_Receiving SALT) Relation_GE (Expr_Lit 10)

faucet = (mempty,) $ sendingFunds Nothing $ fmap Expr_Lit faucetTap

faucetTap :: Map Token Amount
faucetTap = fold
  [ BTC  =: 1
  , ETH  =: 10
  , SALT =: 1000
  , USDC =: 1e3
  , USDT =: 1e3
  ]

forward = (mempty, i)
  where
    i = Condition_Temporary 15 True $ Condition_Any 1
      [ Condition_Signature $ derivePublicKey demoMnemonicA_1
      , swapPolicy
          Nothing
          (BTC  =: Expr_Lit 2)
          (USDC =: Expr_Lit 70000)
      ]

multisig = (mempty,) $ Condition_Any 1
  [ Condition_Restriction $ Binary (Expr_Sending BTC) Relation_LE (Expr_Lit 1)
  , Condition_Any 2 $ fmap (Condition_Signature . derivePublicKey) [demoMnemonicA_2, demoMnemonicB_2, demoMnemonicC_2]
  ]

betRateVar :: Var Double
betRateVar = Var VariableTag_Amount "BTCUSDC"

balanced :: Intent
balanced = Condition_Restriction $ fmap Expr_Balance $ Binary USDC Relation_GE USDT

-- Public deterministic demo fixtures. All derived keys are publicly reproducible.
-- Never fund these accounts or reuse these phrases outside disposable testnets.
demoMnemonicA, demoMnemonicB, demoMnemonicC, demoMnemonicZ :: Text

demoMnemonicA_1, demoMnemonicA_2, demoMnemonicA_3, demoMnemonicA_4, demoMnemonicA_5, demoMnemonicA_6, demoMnemonicA_7 :: SecretKey
demoMnemonicB_1, demoMnemonicB_2, demoMnemonicB_3, demoMnemonicB_4, demoMnemonicB_5, demoMnemonicB_6:: SecretKey
demoMnemonicC_2, demoMnemonicC_3 :: SecretKey
demoMnemonicZ_1, demoMnemonicZ_2, demoMnemonicZ_3 :: SecretKey

demoMnemonicA = "excuse ozone east canoe duck tortoise dentist approve bid wagon area funny"
demoMnemonicB = "vehicle glue talk scissors away blame film spend visit timber wasp hybrid"
demoMnemonicC = "stem note title control cute vapor response bullet liquid knock marble civil"
demoMnemonicZ = "random jaguar area hobby antenna modify spell gesture green permit table cook"

demoMnemonicA_1 = unsafeGenerateTestAccount 0 demoMnemonicA
demoMnemonicA_2 = unsafeGenerateTestAccount 1 demoMnemonicA
demoMnemonicA_3 = unsafeGenerateTestAccount 2 demoMnemonicA
demoMnemonicA_4 = unsafeGenerateTestAccount 3 demoMnemonicA
demoMnemonicA_5 = unsafeGenerateTestAccount 4 demoMnemonicA
demoMnemonicA_6 = unsafeGenerateTestAccount 5 demoMnemonicA
demoMnemonicA_7 = unsafeGenerateTestAccount 6 demoMnemonicA

demoMnemonicB_1 = unsafeGenerateTestAccount 0 demoMnemonicB
demoMnemonicB_2 = unsafeGenerateTestAccount 1 demoMnemonicB
demoMnemonicB_3 = unsafeGenerateTestAccount 2 demoMnemonicB
demoMnemonicB_4 = unsafeGenerateTestAccount 3 demoMnemonicB
demoMnemonicB_5 = unsafeGenerateTestAccount 4 demoMnemonicB
demoMnemonicB_6 = unsafeGenerateTestAccount 5 demoMnemonicB

demoMnemonicC_2 = unsafeGenerateTestAccount 1 demoMnemonicC
demoMnemonicC_3 = unsafeGenerateTestAccount 2 demoMnemonicC

demoMnemonicZ_1 = unsafeGenerateTestAccount 0 demoMnemonicZ
demoMnemonicZ_2 = unsafeGenerateTestAccount 1 demoMnemonicZ
demoMnemonicZ_3 = unsafeGenerateTestAccount 2 demoMnemonicZ
