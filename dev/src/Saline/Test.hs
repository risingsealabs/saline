{-# LANGUAGE OverloadedStrings #-}
module Saline.Test where

import Control.Monad (void)
import Data.Aeson (FromJSON (..), ToJSON (..), fromJSON)
import Data.Foldable
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Some
import Reflex.Dom.Core ((=:))
import Test.QuickCheck (Arbitrary)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (QuickCheckReplay (..), QuickCheckTests (..), testProperty)

import Saline.Intents.Arbitrary
import Saline.Intents.Condition
import Saline.Intents.Instrument qualified as Instrument
import Saline.Intents.Witness
import Saline.Network.CometBFT.Node
import Saline.Network.Node.Transaction
import Saline.Test.Script
import Saline.Utils (tshow)

main, dev :: IO ()
main = run Level_Network
dev = run Level_Handler

run :: Level -> IO ()
run = defaultMain . tests

tests :: Level -> TestTree
tests l = testGroup "All tests"
  [ testGroup "Serialisation roundtrips" $ roundtrips
  , testGroup "Saline scenario tests" $ scenarios l
  ]

roundtrips :: [TestTree]
roundtrips =
  [ t "Token" $ roundtrip @Token
  , t "Relation" $ roundtrip @Relation
  , t "Arithmetic2" $ roundtrip @Arithmetic2
  , t "VariableTag" $ roundtrip @(Some VariableTag)

  , t "Witness" $ roundtrip @Witness
  , t "Expr Amount" $ roundtrip @(Expr Amount)
  , t "Expr Address" $ roundtrip @(Expr Address)
  , t "Condition Expr Expr []" $ roundtrip @(Condition Expr Expr [])
  , t "BridgeInstruction" $ roundtrip @BridgeInstruction
  , t "Instruction" $ roundtrip @Instruction
  , t "Transaction" $ roundtrip @Tx
  , t "Signed" $ roundtrip @SignedTx
  ] where
    t :: (Arbitrary a, Show a) => TestName -> (a -> Bool) -> TestTree
    t n p = testProperty n p
      & localOption (QuickCheckTests 100)
      & localOption (QuickCheckReplayLegacy rngSeed)

roundtrip :: (FromJSON a, ToJSON a, Eq a) => a -> Bool
roundtrip x = fromJSON (toJSON x) == pure x

scenarios :: Level -> [TestTree]
scenarios l =
  [ testGroup "application scenarios" application
  , testGroup "fixed scenarios" fixed
  , testGroup "dynamic scenarios" dynamic
  ]
  where
    t ss f = flip fmap ss $ \(n,s) -> testCase n $ runScript $ f s

    application = case l of
      Level_Handler -> t applicationScenarios $ handlerLevel defCfg
      Level_Network -> t applicationScenarios $ \s -> networkLevel defCfg $ \ns -> do
        startNodes ns >> waitForNodes ns
        s

    fixed = case l of
      Level_Handler -> []
      Level_Network -> t fixedScenarios $ \(cfg, s) -> networkLevel cfg $ \ns -> do
        startNodes ns >> waitForNodes ns
        s ns

    dynamic = case l of
      Level_Handler -> []
      Level_Network -> t dynamicScenarios $ \s -> networkLevel defCfg $ \ns -> s ns

applicationScenarios :: [(TestName, Script l ())]
applicationScenarios =
  [ t "can onboard and offboard assets" onoffboardTest
  , t "can make a simple transfer" transferTest
  , t "can make a simple swap" swapTest
  , t "can set a simple intent" setIntentTest
  , t "can set a simple intent, and add one on top" addIntentTest
  , t "can install an intent and then clear it" clearIntentTest
  , t "can install a large intent" largeIntentTest
  ] where t = (,)


fixedScenarios :: [(TestName, (ScriptCfg, NonEmpty CometBFTNode -> Script Level_Network ()))]
fixedScenarios =
  [ t "can add a node to the network and start it and all" newNodeTest
  , t "can enable and reach prometheus in tendermint" instrumentationTest
  ] where t = (,)

dynamicScenarios :: [(TestName, NonEmpty CometBFTNode -> Script l ())]
dynamicScenarios = []

onoffboardTest :: Script l ()
onoffboardTest = do
  (sk1, pk1) <- newWallet
  tx1 <- onboard pk1 10 ETH
  void $ waitTx tx1
  ethAmount1 <- getBalance pk1 ETH
  checks $ assertEqual "10 ETH successfully onboarded" 10 ethAmount1
  tx2 <- offboard (sk1, pk1) 10 ETH
  void $ waitTx tx2
  ethAmount2 <- getBalance pk1 ETH
  checks $ assertEqual "10 ETH successfully offboarded" 0 ethAmount2

transferTest :: Script l ()
transferTest = do
  (sk1, pk1) <- newWallet
  (_, pk2) <- newWallet
  tx1 <- onboard pk1 10 ETH
  void $ waitTx tx1
  amounts1 <- getBalances pk1
  checks $ assertEqual "10 ETH successfully onboarded" (ETH =: 10) amounts1
  tx2 <- transfer pk1 pk2 (ETH =: 10) (NonEmpty.singleton sk1)
  void $ waitTx tx2
  ethAmountA <- getBalances pk1
  ethAmountB <- getBalances pk2
  checks $ do
    assertEqual "ETH got moved out of first addr" mempty ethAmountA
    assertEqual "ETH got moved into second addr" (ETH =: 10) ethAmountB

swapTest :: Script l ()
swapTest = do
  (sk1, pk1) <- newWallet
  (sk2, pk2) <- newWallet
  tx1 <- onboard pk1 10 ETH
  tx2 <- onboard pk2 2 BTC
  forM_ [tx1, tx2] waitTx
  user1_before <- getBalances pk1
  user2_before <- getBalances pk2
  checks $ do
    assertEqual "user1 starts with 10 ETH and nothing else" (ETH =: 10) user1_before
    assertEqual "user2 starts with 2 BTC and nothing else" (BTC =: 2) user2_before
  tx3 <- swap pk1 (ETH =: 10) pk2 (BTC =: 2) (NonEmpty.fromList [sk1, sk2])
  void $ waitTx tx3
  user1_after <- getBalances pk1
  user2_after <- getBalances pk2
  checks $ do
    assertEqual "user1 ends up with no ETH and 2 BTC" (BTC =: 2) user1_after
    assertEqual "user2 ends up with 10 ETH and no BTC" (ETH =: 10) user2_after

setIntentTest :: Script l ()
setIntentTest = do
  user1 <- newWallet
  tx1 <- setIntent user1 intent1
  void $ waitTx tx1
  r1 <- getIntent (snd user1)
  checks $ assertEqual "user intent was correctly set" (Just intent1) r1

  where intent1 = swapPolicy
          Nothing
          (BTC =: Expr_Lit 2)
          (ETH =: Expr_Lit 10)

addIntentTest :: Script l ()
addIntentTest = do
  user1 <- newWallet
  tx1 <- setIntent user1 intent1
  void $ waitTx tx1
  r2 <- getIntent (snd user1)
  checks $ assertEqual "intent was correctly set" (Just intent1) r2
  tx2 <- addIntent user1 intent2
  void $ waitTx tx2
  r3 <- getIntent (snd user1)
  checks $ assertBool "intent was correctly added on top" (r3 == Just intent3 || r3 == Just intent3')

  where intent1 = swapPolicy
          Nothing
          (BTC =: Expr_Lit 2)
          (ETH =: Expr_Lit 10)
        intent2 = swapPolicy
          Nothing
          (USDT =: Expr_Lit 4000)
          (ETH =: Expr_Lit 1)
        intent3  = Condition_Any 1 [intent1, intent2]
        intent3' = Condition_Any 1 [intent2, intent1]

clearIntentTest :: Script l ()
clearIntentTest = do
  user1 <- newWallet
  tx1 <- setIntent user1 intent
  void $ waitTx tx1
  i1 <- getIntent (snd user1)
  checks $ assertEqual "intermediate state has intent installed" (Just intent) i1
  tx2 <- clearIntent user1
  void $ waitTx tx2
  i2 <- getIntent (snd user1)
  checks $ assertEqual "final state is intent free" Nothing i2

  where intent = swapPolicy
          Nothing
          (BTC =: Expr_Lit 2)
          (ETH =: Expr_Lit 10)

largeIntentTest :: Script l ()
largeIntentTest = do
  let i0 = snd Instrument.limited
  user1 <- newWallet
  void $ waitTx =<< setIntent user1 i0
  i1 <- getIntent (snd user1)
  checks $ assertEqual "large intent was installed" (Just i0) i1

instrumentationTest :: (ScriptCfg, NonEmpty CometBFTNode -> Script Level_Network ())
instrumentationTest = (defCfg { instrumented = True },) $ \ns -> do
  ms <- getPrometheusMetrics (NonEmpty.head ns)
  info "Prometheus metrics" (Map.keys ms)

newNodeTest :: (ScriptCfg, NonEmpty CometBFTNode -> Script Level_Network ())
newNodeTest = (defCfg,) $ \ns -> do
  info "existing nodes info" [ tshow n | n <- toList ns ]
  sleep 5
  n <- newNode
  info "new node info" [ tshow n ]
  startNode n >> waitForNode n
  h <- nodeHealth n
  info "new node health" [ tshow h ]
