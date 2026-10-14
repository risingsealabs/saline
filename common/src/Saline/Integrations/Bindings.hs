{-# LANGUAGE OverloadedStrings #-}
module Saline.Integrations.Bindings where

import Prelude hiding (lines, unlines, writeFile)

import Control.Lens (Iso', iso)
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Invariant
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Some
import Data.Text (Text)
import Data.Time (DiffTime)
import Data.UUID (UUID)
import Data.Universe (Finite (..))
import Data.Word

import Saline.Crypto
import Saline.Utils (AesonMap, Monoidal (..), isomap)

-- Combinators for describing multiple consistent serialisations

type Sig = [Argument]

type Argument = (Name, Maybe Hint)

type Name = Text

type Hint = Some Binding

data Binding a where
  Binding_Bool :: Binding Bool
  Binding_Str :: Binding Text
  Binding_Time :: Binding DiffTime
  Binding_Optional :: Binding a -> Binding (Maybe a)
  Binding_List :: Binding a -> Binding [a]
  Binding_NEList :: Binding a -> Binding (NonEmpty a)
  Binding_Dict :: Binding k -> Binding v -> Binding (Map k v) -- TODO: which map?
  Binding_Dict' :: Binding k -> Binding v -> Binding (AesonMap k v) -- TODO: which map?
  Binding_Defined :: Text -> Binding a
  Binding_Imported :: Text -> Binding a
  Binding_Generic :: Binding a --TODO: make typesafe

optionalType :: Binding a -> Binding (Maybe a)
optionalType = Binding_Optional

listType :: Binding a -> Binding [a]
listType = Binding_List

list1Type :: Binding a -> Binding (NonEmpty a)
list1Type = Binding_NEList

dictType :: Binding k -> Binding v -> Binding (Map k v)
dictType = Binding_Dict

dictType' :: Binding k -> Binding v -> Binding (AesonMap k v)
dictType' = Binding_Dict'

strType :: Binding Text
strType = Binding_Str

boolType :: Binding Bool
boolType = Binding_Bool

timeType :: Binding DiffTime
timeType = Binding_Time

genericArg :: Binding a
genericArg = Binding_Generic

--TODO: obtain witness from imports
word64Type :: Binding Word64
word64Type = Binding_Imported "uint64"

float64Type :: Binding Double
float64Type = Binding_Imported "float64"

uuidType :: Binding UUID
uuidType = Binding_Imported "UUID"

publicKeyType :: Binding PublicKey
publicKeyType = Binding_Imported "G2Element"

signatureType :: Binding Signature
signatureType = Binding_Imported "G1Element"


data Named a = Named
  { _named_name  :: Name
  , _named_value :: a
  }
  deriving stock (Functor)

named :: Name -> a -> Named a
named = Named

data Enumeration a = Finite a => Enumeration
  { _enumeration_caseName :: (a -> Name)
  }

enumeration :: Finite a => (a -> Name) -> Enumeration a
enumeration = Enumeration

-- Tagged sum of products
data Union (f :: Type -> Type) = Union
  { _union_structs :: forall x. f x -> Named (Struct x) --TODO: parametrize?
  }

union :: (forall x. f x -> Named (Struct x)) -> Union f
union = Union

data Struct x where
  Struct_Field :: (FromJSON x, ToJSON x) => Named (Binding x) -> Struct x --TODO: pull out constraint
  Struct_Unit :: Struct ()
  Struct_Pair :: Struct x -> Struct y -> Struct (x,y)
  Struct_Iso :: Iso' x y -> Struct x -> Struct y
  Struct_TODO :: Struct x --TODO: remove

instance Invariant Struct where
  invmap fw bw = Struct_Iso $ iso fw bw

instance Monoidal Struct where
  unit = Struct_Unit
  pair = Struct_Pair

data StructEvaluator f = StructEvaluator
  { _structEvaluator_field :: forall x. (FromJSON x, ToJSON x) => Named (Binding x) -> f x
  , _structEvaluator_todo  :: forall x. f x
  }

evalStruct :: forall f a. Monoidal f => StructEvaluator f -> Struct a -> f a
evalStruct h = go
  where
    go :: forall x. Struct x -> f x
    go = \case
      Struct_Unit -> unit
      Struct_Field n -> _structEvaluator_field h n
      Struct_Pair x y -> pair (go x) (go y)
      Struct_Iso i x -> isomap i (go x)
      Struct_TODO -> _structEvaluator_todo h

field :: (FromJSON a, ToJSON a) => Name -> Binding a -> Struct a
field n = Struct_Field . Named n

--TODO: remove
structTODO :: Struct a
structTODO = Struct_TODO
