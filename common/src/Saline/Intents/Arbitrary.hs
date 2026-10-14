{-# OPTIONS_GHC -Wno-incomplete-patterns #-} --TODO: remove
{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Saline.Intents.Arbitrary where

import Control.Applicative
import Control.Lens (Wrapped (Unwrapped), view, _Unwrapped')
import Control.Monad (replicateM)
import Control.Monad.State (evalState)
import Data.ByteArray (ByteArray)
import Data.ByteArray qualified as ByteArray
import Data.ByteArray.Sized (SizedByteArray, unsafeSizedByteArray)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Char (ord)
import Data.Dependent.Sum (DSum ((:=>)))
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Proxy
import Data.Some (Some (..), withSome)
import Data.Time (DiffTime)
import Data.Universe
import Data.Universe.Some
import GHC.TypeNats
import Test.QuickCheck hiding (generate)
import Test.QuickCheck.Instances.Semigroup ()
import Test.QuickCheck.Instances.UUID ()
import Test.QuickCheck.Random (mkQCGen)

import Saline.Crypto
import Saline.Intents.Condition
import Saline.Intents.Witness
import Saline.Network.Node.Transaction
import Saline.Utils (NewtypeDSum (NewtypeDSum), generate)

rngSeed :: Int
rngSeed = 123456789

wrapArbitrary :: (Wrapped b, Arbitrary (Unwrapped b)) => Gen b
wrapArbitrary = fmap (view _Unwrapped') arbitrary

instance Arbitrary SignedTx where
  arbitrary = do
    (nonce,tx) <- arbitrary
    key <- arbitrary
    pure $ mkSignedTxWithNonce nonce (tx,key)

instance Arbitrary Tx where
  arbitrary = pure Tx
    <*> insts
    <*> pure mempty -- TODO
    <*> pure mempty -- TODO
    <*> pure mempty -- TODO
    where
      -- `arbitrary` instance generates larger lists which takes more time
      -- TODO: be more thorough on actual test suite than `make test`
      insts = liftA2 (:|) arbitrary $ replicateMM (chooseEnum (0,4)) arbitrary

instance Arbitrary Witness where
  arbitrary = oneof
    [ pure Witness_Auto
    , pure Witness_All <*> children
    , pure Witness_Any <*> fmap (Map.fromList . zip [0..]) children
    ]
    where
      children = replicateMM (chooseEnum (0,3)) arbitrary

instance Arbitrary Instruction where
  arbitrary = dsum (elements universeF) $ \case
    InstructionTag_Funds -> liftA3 (,,) arbitrary arbitrary funds
    InstructionTag_OrIntent -> arbitrary
    InstructionTag_SetIntent -> arbitrary
    InstructionTag_Delete -> arbitrary
    InstructionTag_Bridge -> arbitrary
    where
      funds :: Gen (Map Token Amount)
      funds = do
        mp <- arbitrary @(Map Token Amount)
        sequence $ amount <$ mp

instance Arbitrary BridgeInstruction where
  arbitrary = dsum (elements universeF) $ \case
    BridgeInstructionTag_Burn -> liftA2 (,) arbitrary amount
    BridgeInstructionTag_Mint -> liftA3 (,,) arbitrary arbitrary amount

instance (Arbitrary a, Arbitrary op) => Arbitrary (Binary op a) where
  arbitrary = wrapArbitrary

instance Arbitrary (Condition Expr Expr []) where
  arbitrary = tree 2 go
    where
      go :: Gen [(Condition Expr Expr [])] -> Gen (Condition Expr Expr [])
      go branch = dsum arbitrary $ \case
        ConditionTag_All -> branch
        ConditionTag_Any -> liftA2 (,) arbitrary branch
        ConditionTag_Counterparty -> arbitrary
        ConditionTag_Signature -> arbitrary
        ConditionTag_Restriction -> arbitrary
        ConditionTag_Finite -> arbitrary
        ConditionTag_Temporary -> arbitrary

instance Arbitrary (Expr Amount) where
  arbitrary = fmap NewtypeDSum $ dsum arbitrary $ \case
    ExprTag_Lit -> amount
    ExprTag_Balance -> arbitrary
    ExprTag_Var -> var VariableTag_Amount
    ExprTag_Send -> arbitrary
    ExprTag_Receive -> arbitrary
    ExprTag_Arithmetic2 -> wrapArbitrary

--TODO: check for negatives or stop using Double
amount :: Gen Double
amount = fromIntegral @Word @Double <$> choose (0, 1e6)

instance Arbitrary DiffTime where
  arbitrary = oneof
    [ fmap fromRational arbitrary
    , fmap fromInteger $ chooseInteger (-1e50, 1e50)
    ]

var :: VariableTag a -> Gen (Var a)
var tg = Var tg <$> elements ["Foo", "Bar", "Baz"]

instance Arbitrary (Var Address) where arbitrary = var VariableTag_Address
instance Arbitrary (Var Amount)  where arbitrary = var VariableTag_Amount

instance Arbitrary (Expr Address) where
  arbitrary = oneof
    [ fmap Expr_Lit arbitrary
    , fmap Expr_Var arbitrary
    ]

instance Finite (VariableTag a) => Arbitrary (VariableTag a) where
  arbitrary = elements universeF

instance (forall a. Finite (VariableTag a)) => Arbitrary (Some Var) where
  arbitrary = do
    t <- arbitrary
    fmap Some $ var t

instance Arbitrary (Some VariableTag) where
  arbitrary = elements universeF

instance FiniteSome (f a) => Arbitrary (Some (f a)) where
  arbitrary = elements universeF

instance Arbitrary PublicKey where
  arbitrary = elements memoizedKeys
    where
      memoizedKeys = flip evalState (mkQCGen rngSeed) $ generate $ vectorOf 100 $ derivePublicKey <$> arbitrary

instance Arbitrary SecretKey where
  arbitrary = secretKeyFromSeed <$> arbitrary

instance (Arbitrary ba, ByteArray ba, KnownNat n) => Arbitrary (SizedByteArray n ba) where
  arbitrary = unsafeSizedByteArray . ByteArray.pack <$> vectorOf size char
    where
      char = toEnum . ord <$> elements ['a'..'z']
      size = fromEnum $ natVal @n Proxy

instance Arbitrary ByteString where
  arbitrary = fmap ByteString.pack arbitrary

instance Arbitrary Arithmetic2 where arbitrary = elements universeF
instance Arbitrary Relation where arbitrary = elements universeF
instance Arbitrary Token    where arbitrary = elements universeF

tree :: Word -> (Gen [a] -> Gen a) -> Gen a
tree maxBranches f = go maxBranches
  where
    go m = f $ ireplicate (chooseEnum (0, m)) $ \_ -> do
      go =<< chooseEnum (0, m)

ireplicate :: Gen Word -> (Word -> Gen a) -> Gen [a]
ireplicate gn ga = do
  n <- gn
  replicateM (fromEnum n) (ga n)

--TODO: better name, generalise
replicateMM :: Gen Word -> Gen a -> Gen [a]
replicateMM an aa = do
  n <- an
  replicateM (fromEnum n) aa

dsum :: Gen (Some k) -> (forall x. k x -> Gen x) -> Gen (DSum k Identity)
dsum ka fkv = do
  sk <- ka
  withSome sk $ \k -> do
    v <- fkv k
    pure $ k :=> Identity v
