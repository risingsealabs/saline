{-# LANGUAGE CPP #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

{-# LANGUAGE NoMonomorphismRestriction #-}

{- import Saline.Intents.Satisfying qualified as Sat -}
module Saline.Intents.Satisfying where

import Control.Category ((>>>))
import Control.Error.Util (note)
import Control.Lens qualified as Lens
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Dependent.Map qualified as DMap
import Data.Dependent.Sum
import Data.Either
import Data.Foldable hiding (all)
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Merge.Lazy qualified as Map
import Data.Some
import Data.Text (Text)
import Data.Traversable
import Obelisk.Route (tryDecode)
import Test.QuickCheck hiding (arbitrary, choose)

import Saline.Crypto (publicKeyEncoder, secretKeyFromSeed, derivePublicKey)
import Saline.Intents.Condition
import Saline.Network.Node.Transaction (lookupVar)
import Saline.Utils
import Saline.Utils.Satisfying (Satisfying(..))
import Saline.Utils.Satisfying qualified as Sat

-- TODO: pretty print messages
data UnsatisfiableReason
  = UnsatisfiableReason_VariableNotFound (Some VariableTag) Text
  | UnsatisfiableReason_NotEnoughElements
  | UnsatisfiableReason_NotEnoughSatisfiableElements
  | UnsatisfiableReason_NotYetImplemented
  deriving stock (Eq, Ord, Show)

-- Conditions
variable :: VariableTag a -> Satisfying a
variable = \case
  VariableTag_Address -> address
  VariableTag_Amount  -> Sat.choose (1, 1000)
  VariableTag_Count   -> Sat.choose (1, 10)
  VariableTag_List    -> Sat.replicate (Sat.choose (2, 6)) $ variable VariableTag_Address

token :: Token -> Satisfying Token
token = Sat.equals

-- TODO: relax constraints based on operation
-- TODO: should we just evalExpr ?
arithmetic2 :: (Eq a, Fractional a) => VariableValues -> Arithmetic2 -> Expr a -> Expr a -> Either UnsatisfiableReason (Satisfying a)
arithmetic2 vals op e a = flip (f op) <$> expr vals e <*> evalExpr vals a
  where
    f = \case
      Arithmetic2_Add -> Sat.add
      Arithmetic2_Div -> Sat.div
      Arithmetic2_Mul -> Sat.mul
      Arithmetic2_Sub -> Sat.sub

-- TODO: merge with expr?
evalExpr :: Eq a => VariableValues -> Expr a -> Either UnsatisfiableReason a
evalExpr vals = unNewtypeDSum >>> \case
  ExprTag_Receive :=> Identity _ -> Left UnsatisfiableReason_NotYetImplemented
  ExprTag_Send :=> Identity _ -> Left UnsatisfiableReason_NotYetImplemented
  ExprTag_Lit :=> Identity a -> pure a
  ExprTag_Balance :=> Identity _ -> Left UnsatisfiableReason_NotYetImplemented
  ExprTag_Var :=> Identity v -> lookupVar' v vals
  ExprTag_Arithmetic2 :=> Identity (Binary e op a) -> evalOp op <$> evalExpr vals e <*> evalExpr vals a

lookupVar' :: Var a -> VariableValues -> Either UnsatisfiableReason a
lookupVar' v@(Var tag name) = note (UnsatisfiableReason_VariableNotFound (Some tag) name) . lookupVar v

insertVar :: (VariableTag a, Identifier) -> a -> VariableValues -> VariableValues
insertVar (tag, name) val = DMap.insertWith merge tag $ Compose $ Map.singleton name $ Identity val
  where merge (Compose a) (Compose b) = Compose (a <> b) -- TODO: collisions?

var :: Eq a => VariableValues -> Var a -> Either UnsatisfiableReason (Satisfying a)
var vals v = fmap Sat.equals $ lookupVar' v vals

-- TODO: can't seem to write with pattern synonyms as inference breaks
expr :: Eq a => VariableValues -> Expr a -> Either UnsatisfiableReason (Satisfying a)
expr vals (NewtypeDSum e) = case e of
  ExprTag_Receive :=> Identity _ -> Left UnsatisfiableReason_NotYetImplemented
  ExprTag_Send :=> Identity _ -> Left UnsatisfiableReason_NotYetImplemented
  ExprTag_Lit :=> Identity a -> pure $ Sat.equals a
  ExprTag_Balance :=> Identity _ -> Left UnsatisfiableReason_NotYetImplemented
  ExprTag_Var :=> Identity v -> var vals v
  ExprTag_Arithmetic2 :=> Identity (Binary ex op a) -> arithmetic2 vals op ex a

asset :: VariableValues -> (,) Token (Expr Amount) -> Either UnsatisfiableReason (Satisfying ((,) Token Amount))
asset vals (t,e) = pair (Sat.equals t) <$> expr vals e

-- TODO: dedup with `asset`?
-- TODO: use sequence?
funds :: VariableValues -> Map Token (Expr Amount) -> Either UnsatisfiableReason (Satisfying (Map Token Amount))
funds vals exprs = do
  sats <- for exprs $ expr vals
  pure $ Satisfying
    { _satisfying_gen       = for sats _satisfying_gen
    , _satisfying_predicate = all (maybe False $ uncurry _satisfying_predicate) . sameKeysOnly sats
    , _satisfying_shrink    = traverse shrink
    } where sameKeysOnly = Map.merge (pure Nothing) (pure Nothing) $ Map.zipWithMatched $ \_ x y -> Just (x,y)

address :: Satisfying Address
address = Satisfying
  { _satisfying_gen       = derivePublicKey . secretKeyFromSeed <$> _satisfying_gen Sat.sizedByteArray
  , _satisfying_predicate = const True
  , _satisfying_shrink    = pure
  }

transfer
  :: (VariableValues -> assetsExpr -> Either UnsatisfiableReason (Satisfying assets))
  -> VariableValues
  -> Transfer assetsExpr Expr
  -> Either UnsatisfiableReason (Satisfying (Transfer assets Identity))
transfer assets vals tr = do
  counterparty <- expr vals (_transfer_counterparty tr)
  a <- assets vals (_transfer_assets tr)
  let r = Sat.equals $ _transfer_hostRole tr
  pure $ Sat.iso Lens._Unwrapped' $ triple (Sat.identity counterparty) r a

-- TODO: use Data.Either.Validation to accumulate errors?
-- TODO: some conditions are *always* unsatisfiable, regardless of variable instantiation
condition :: VariableValues -> Condition Expr Expr [] -> Either UnsatisfiableReason (Satisfying (Condition Identity Identity []))
condition _ = go
  where
    go = \case
      Condition_Finite _ c    -> go c
      Condition_Temporary _ _ c -> go c
      Condition_Restriction {} -> Left UnsatisfiableReason_NotYetImplemented
      Condition_Counterparty p -> pure $ Sat.prism _Condition_Counterparty $ Sat.equals p
      Condition_Signature p    -> pure $ Sat.prism _Condition_Signature $ Sat.equals p
      Condition_All cs         -> Sat.prism _Condition_All . Sat.sequence <$> (sequence =<< evalList' cs)
      Condition_Any k ps'      -> do
        ps <- evalList' ps'

        let
          (_unsats, sats) = partitionEithers ps

          tooSmall :: [b] -> Bool
          tooSmall = (k >) . toEnum . length

        --TODO: more context
        when (tooSmall ps)   $ Left UnsatisfiableReason_NotEnoughElements
        when (tooSmall sats) $ Left UnsatisfiableReason_NotEnoughSatisfiableElements

        -- TODO: can have anywhere between `k` and `length sats`
        Right $ Sat.prism _Condition_Any $ pair (Sat.equals k) $ Sat.sequence sats
{-
    evalList :: List (Condition Expr Expr List) -> Either UnsatisfiableReason [Either UnsatisfiableReason (Satisfying (Condition Identity Identity []))]
    evalList = \case
      List_Lit as -> evalList' as
      List_For i xs a -> flip (fmap . fmap) (lookupVar' xs vals) $ \x ->
        condition (insertVar (VariableTag_Address, i) x vals) a
-}
    evalList' :: [Condition Expr Expr []] -> Either UnsatisfiableReason [Either UnsatisfiableReason (Satisfying (Condition Identity Identity []))]
    evalList' = pure . fmap go

test :: Show a => Int -> Int -> Satisfying a -> IO ()
test rounds size s = quickCheckWith args $ forAllShrink (_satisfying_gen s) (_satisfying_shrink s) (_satisfying_predicate s)
  where
    args = stdArgs
      { maxSuccess = rounds
      , maxSize = size
      }

-- TODO: proper test suite
suite :: IO ()
suite = do
  let
    --TODO: typesafe
    party1 = fromRight (error "suite: party1") $ tryDecode publicKeyEncoder
      "nacl:0xf27464bff119fbd1927c1eb6df2d54789e9f510d3434a9075603436f3a9f7cd9a164acf7de39faa30726b0b4cdb21c0039441f646cc565ba2e4f63b3971b21cf183294cd51eb09a7d3a1c93716f0c21f74f210b45710a85bed77718f451c4d0e"
    intent = fromRight (error "suite: intent") $ tryDecode publicKeyEncoder
      "nacl:0x6ce4877e268777b42cee2291ee9db8d4ed0a5c14aebbafc02ac217ecc65c1dc0a128226b066eb1ee9526d9f55e0b760e8d38f4da3acb82d785de86cc7127059cc6040887b9ebc38340025266aebb54df67e9fceddcf3ebc68ccf3d01d1d4dc02"

    --TODO: typesafe lookup
    vals = DMap.fromList
      [ f VariableTag_Amount
        [ ("x", Identity 123)
        ]
      , f VariableTag_Address
        [ ("party1", Identity party1)
        , ("intent", Identity intent)
        ]
      ] where f tag vs = tag :=> Compose (Map.fromList vs)

    as = Map.fromList
      [ (BTC, Expr_Lit 10)
      , (ETH, Expr_Var (Var VariableTag_Amount "x"))
      ]
    p1 = Expr_Var $ Var VariableTag_Address "party1"
    tr = Transfer p1 Role_Sink as

    c_true = Condition_All []
    c_false = Condition_Any 1 []

  do
    let
      t = test 1e4 1e9
      t' f x = case f vals x of
        Right s -> t s
        Left err -> do
          putStrLn ""
          putStrLn $ show err <> ": " <> show x

    t $ Sat.even @Int
    t $ Sat.odd @Int
    t $ Sat.positive @Int
    t $ Sat.negative @Int
    t $ Sat.multiple @Int 13 Sat.arbitrary
    t $ Sat.multiple @Int (-22) Sat.positive
    t $ pair @_ @Int @Int Sat.positive Sat.negative
    t $ Sat.choose @Int (-1e18, 1e18)
    t $ Sat.choose @Word (0, 1e19)
    t $ Sat.replicate (Sat.choose (1,20)) $ Sat.even @Word
    t $ Sat.sizedByteArray @ByteString @40
    t address
    t' funds as
    t' (transfer funds) tr

    let
      cs =
        [ c_true
        , c_false
        ]

    for_ cs $ t' condition
    for_ [1,2,3] $ \i ->
      t' condition $ Condition_Any (toEnum i) cs

    putStrLn ""

  do
    let t = test 1e3 1e3
    t $ Sat.all $ Sat.positive @Int

  putStrLn "Done"
