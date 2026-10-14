{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Saline.Intents.Condition where

import Control.Lens (Prism', Wrapped (..), iso, makeLenses, _Unwrapped')
import Control.Monad
import Data.Aeson
import Data.Aeson.GADT.TH (deriveJSONGADT)
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Constraint.Extras (Has (has), Has', has', whichever)
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Constraint.Forall (ForallF)
import Data.Dependent.Map (DMap)
import Data.Dependent.Map qualified as DMap
import Data.Dependent.Sum
import Data.Foldable
import Data.Functor.Compose
import Data.Functor.Const
import Data.Functor.Identity
import Data.GADT.Compare
import Data.Kind
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Some
import Data.Text (Text)
import Data.Time
import Data.Type.Equality
import Data.Universe
import Data.Universe.Some
import Data.Universe.Some.TH
import Data.Word (Word64)
import GHC.Generics
import Witherable (catMaybes)

import Saline.Crypto (PublicKey)
import Saline.Integrations.Aeson
import Saline.Integrations.Bindings
import Saline.Utils
import Saline.Utils.TH

type Address = PublicKey

type Amount = Double

type Identifier = Text

type Duration = DiffTime
type Timestamp = DiffTime

type Threshold = Word64

data Token
  = BTC
  | ETH
  | USDC
  | USDT
  | SALT
  deriving stock (Eq, Ord, Read, Show, Generic, Enum, Bounded)
  deriving anyclass (Universe, Finite)
  deriving anyclass (FromJSON, FromJSONKey, ToJSON, ToJSONKey)

type AssetF f b = b Token (f Amount)
type Asset b = b Token Amount

data Arithmetic2
  = Arithmetic2_Add
  | Arithmetic2_Div
  | Arithmetic2_Mul
  | Arithmetic2_Sub
  deriving stock (Eq, Ord, Show, Generic, Enum, Bounded)
  deriving anyclass (Universe, Finite)

data Oracle a = Oracle
  { _oracle_address :: PublicKey
  , _oracle_variable :: Var a
  , _oracle_timestamp :: Timestamp
  }
  deriving stock Generic

data ExprTag lit a where
  ExprTag_Lit :: ExprTag a a
  ExprTag_Balance :: ExprTag Amount Token
  ExprTag_Send :: ExprTag Amount Token
  ExprTag_Receive :: ExprTag Amount Token
  ExprTag_Var :: ExprTag a (Var a)
  ExprTag_Arithmetic2 :: Fractional a => ExprTag a (Binary Arithmetic2 (Expr a))

type Expr = NewtypeDSum ExprTag Identity

data VariableTag a where
  VariableTag_Address :: VariableTag Address
  VariableTag_Amount  :: VariableTag Amount
  VariableTag_Count   :: VariableTag Word
  VariableTag_List    :: VariableTag [Address] --TODO: generalise

data Var a = Var (VariableTag a) Text
  deriving stock Generic

type VariableName   = DSum VariableTag (Const Text)
type VariableValue  = DSum VariableTag (Compose ((,) Text) Identity)
type VariableNames  = DMap VariableTag (Const   (Set Text))
type VariableValues = DMap VariableTag (Compose (Map Text) Identity)

--TODO: DRY
instance (ForallF ToJSON VariableTag, Has' ToJSON VariableTag g) => ToJSON (DSum VariableTag g) where
  toJSON ((f :: f a) :=> (g :: g a))
    = whichever @ToJSON @f @a (has' @ToJSON @g f (toJSON (f, g)))
instance (FromJSON (Some VariableTag), Has' FromJSON VariableTag g) => FromJSON (DSum VariableTag g) where
  parseJSON x = do
    (jf, jg) <- parseJSON x
    withSomeM (parseJSON jf) $ \(f :: f a) -> do
      g <- has' @FromJSON @g f (parseJSON jg)
      return $ f :=> g

data ListTag elem a where
  ListTag_Lit :: ListTag elem [elem]
  ListTag_For :: ListTag elem (Identifier, Var [Address], elem)

type List = NewtypeDSum ListTag Identity

type Counterparty = Maybe Address --TODO: refine mint/burn/bridge

data Transfer assets party = Transfer
  { _transfer_counterparty :: party Address --TODO: use Counterparty
  , _transfer_hostRole     :: Role
  , _transfer_assets       :: assets
  }
  deriving stock (Generic)

data Role
  = Role_Sink
  | Role_Source
  deriving stock (Eq, Ord, Show, Generic, Enum, Bounded)
  deriving anyclass (Universe, Finite)
  deriving anyclass (FromJSON, FromJSONKey, ToJSON, ToJSONKey)

type Issuance e p l = Transfer (Condition e p l) p

type Rights e p l = (Condition e p l, p Address)

type Uses = Word64

--TODO: Should we normalize to EQ+LT or so?
data Relation
  = Relation_EQ
  | Relation_LT
  | Relation_LE
  | Relation_GT
  | Relation_GE
  deriving stock (Eq, Ord, Read, Show, Generic, Enum, Bounded)
  deriving anyclass (Universe, Finite)

data Binary op a = Binary
  { _binary_lhs :: a
  , _binary_op :: op
  , _binary_rhs :: a
  }
  deriving stock (Foldable, Functor, Generic, Traversable)

--TODO: prevent adjacent applications of limitations?
data ConditionTag expr party list a where
  ConditionTag_All          :: ConditionTag e p l (l (Condition e p l))
  ConditionTag_Any          :: ConditionTag e p l (Threshold, l (Condition e p l))
  ConditionTag_Counterparty :: ConditionTag e p l Address
  ConditionTag_Restriction  :: ConditionTag e p l (Binary Relation (e Amount))
  ConditionTag_Finite       :: ConditionTag e p l (Uses, (Condition e p l))
  ConditionTag_Temporary    :: ConditionTag e p l (Duration, Bool, (Condition e p l))
  ConditionTag_Signature    :: ConditionTag e p l Address

type Condition expr party list = DSum (ConditionTag expr party list) Identity

-- Pattern synonyms

{-# COMPLETE List_Lit, List_For #-}

pattern List_Lit :: [elem] -> List elem
pattern List_Lit a = NewtypeDSum (ListTag_Lit :=> Identity a)

pattern List_For :: Identifier -> Var [Address] -> elem -> List elem
pattern List_For i xs a = NewtypeDSum (ListTag_For :=> Identity (i, xs, a))

{-# COMPLETE Expr_Lit, Expr_Balance, Expr_Var, Expr_Arithmetic2, Expr_Receiving, Expr_Sending #-}

pattern Expr_Lit :: lit -> Expr lit
pattern Expr_Lit lit = NewtypeDSum (ExprTag_Lit :=> Identity lit)

pattern Expr_Balance :: Token -> Expr Amount
pattern Expr_Balance t = NewtypeDSum (ExprTag_Balance :=> Identity t)

pattern Expr_Receiving :: Token -> Expr Amount
pattern Expr_Receiving t = NewtypeDSum (ExprTag_Receive :=> Identity t)

pattern Expr_Sending :: Token -> Expr Amount
pattern Expr_Sending t = NewtypeDSum (ExprTag_Send :=> Identity t)

pattern Expr_Var :: Var lit -> Expr lit
pattern Expr_Var v = NewtypeDSum (ExprTag_Var :=> Identity v)

pattern Expr_Arithmetic2 :: () => Fractional a => (Binary Arithmetic2 (Expr a)) -> Expr a
pattern Expr_Arithmetic2 b = NewtypeDSum (ExprTag_Arithmetic2 :=> Identity b)

{-# COMPLETE Condition_All, Condition_Any, Condition_Counterparty, Condition_Finite, Condition_Temporary, Condition_Restriction, Condition_Signature #-}

pattern Condition_All :: l (Condition e p l) -> Condition e p l
pattern Condition_All ps = ConditionTag_All :=> Identity ps

pattern Condition_Any :: Threshold -> l (Condition e p l) -> Condition e p l
pattern Condition_Any k ps = ConditionTag_Any :=> Identity (k, ps)

pattern Condition_Counterparty :: Address -> Condition e p l
pattern Condition_Counterparty p = ConditionTag_Counterparty :=> Identity p

pattern Condition_Finite :: Uses -> Condition e p l -> Condition e p l
pattern Condition_Finite n c = ConditionTag_Finite :=> Identity (n,c)

pattern Condition_Temporary :: Duration -> Bool -> Condition e p l -> Condition e p l
pattern Condition_Temporary d after c = ConditionTag_Temporary :=> Identity (d, after, c)

pattern Condition_Restriction :: Binary Relation (e Amount) -> Condition e p l
pattern Condition_Restriction r = ConditionTag_Restriction :=> Identity r

pattern Condition_Signature :: Address -> Condition e p l
pattern Condition_Signature p = ConditionTag_Signature :=> Identity p

-- Utils

tokenVar :: Token -> Text
tokenVar = tshow

symbol :: Arithmetic2 -> Text
symbol = \case
  Arithmetic2_Add -> "+"
  Arithmetic2_Div -> "/"
  Arithmetic2_Mul -> "*"
  Arithmetic2_Sub -> "-"

ppRelation :: Relation -> Text
ppRelation = \case
  Relation_EQ -> "="
  Relation_LT -> "<"
  Relation_LE -> "<="
  Relation_GT -> ">"
  Relation_GE -> ">="

evalRelation :: Ord a => Relation -> (a -> a -> Bool)
evalRelation = \case
  Relation_EQ -> (==)
  Relation_LT -> (<)
  Relation_LE -> (<=)
  Relation_GT -> (>)
  Relation_GE -> (>=)

flipRelation :: Relation -> Relation
flipRelation = \case
  Relation_EQ -> Relation_EQ
  Relation_LT -> Relation_GT
  Relation_LE -> Relation_GE
  Relation_GT -> Relation_LT
  Relation_GE -> Relation_LE

flipRestriction :: Binary Relation a -> Binary Relation a
flipRestriction (Binary lhs rel rhs) = Binary rhs (flipRelation rel) lhs

-- TODO: allow multiple constructors with same index?
parseJSONViaSome
  :: (FromJSON (Some f), GEq f, Show (f a))
  => f a -> Value -> Parser (f a)
parseJSONViaSome tag v = do
  Some p <- parseJSON v
  case p `geq` tag of
    Nothing -> typeMismatch (show tag) v
    Just Refl -> pure p

parseJSONSome :: (forall a. Show (f a)) => [Some f] -> Value -> Parser (Some f)
parseJSONSome tags val = do
  (k,v) <- parseJSON val
  flip (withArray "empty array") v $ guard . null
  flip (withText "tag") k $ \txt -> maybe (fail "Unknown tag") pure $ flip find tags $ \(Some t) -> txt == tshow t

makeLenses 'Transfer

-- Instances

deriveMany [id]
  [ deriveArgDict      ''VariableTag

  , deriveStockGADT    ''ConditionTag
  , deriveStockGADT    ''ExprTag
  , deriveStockGADT    ''ListTag
  , deriveStockGADT    ''VariableTag

  , deriveJSONGADT     ''ConditionTag
  , deriveJSONGADT     ''ListTag

  , deriveUniverseSome =<< [d| instance UniverseSome (ConditionTag e p l) |]
  , deriveUniverseSome =<< [d| instance UniverseSome (ListTag elem) |]
  , deriveUniverseSome ''VariableTag
  ]

-- Variable

instance FiniteSome VariableTag

deriving stock    instance Eq        (VariableTag a)
deriving stock    instance Ord       (VariableTag a)
deriving stock    instance Show      (VariableTag a)
deriving anyclass instance ToJSONKey (VariableTag a)

instance FromJSON (VariableTag [Address]) where parseJSON = parseJSONViaSome VariableTag_List
instance FromJSON (VariableTag Address)   where parseJSON = parseJSONViaSome VariableTag_Address
instance FromJSON (VariableTag Amount)    where parseJSON = parseJSONViaSome VariableTag_Amount
instance FromJSON (VariableTag Word)      where parseJSON = parseJSONViaSome VariableTag_Count

deriving stock    instance Eq   (Var a)
deriving stock    instance Ord  (Var a)
deriving stock    instance Show (Var a)

deriving anyclass instance FromJSON (VariableTag a) => FromJSONKey (VariableTag a)

deriving stock    instance Eq   (Oracle a)
deriving stock    instance Ord  (Oracle a)
deriving stock    instance Show (Oracle a)

instance FiniteSome (ListTag elem)

instance (c [elem], c (Identifier, Var [Address], elem)) => Has c (ListTag elem) where
  has = \case
    ListTag_Lit -> id
    ListTag_For -> id

instance Functor List where
  fmap f = \case
    List_Lit as -> List_Lit $ fmap f as
    List_For v i a -> List_For v i (f a)

-- Expr

type ExprConstraints :: Type -> (Type -> Constraint) -> Constraint
type ExprConstraints lit c =
  ( c lit
  , c Token
  , c (Var lit)
  , c (Oracle lit)
  , c (Binary Arithmetic2 (Expr lit))
  , c (Address, Var lit, Timestamp)
  )
instance ExprConstraints lit c => Has c (ExprTag lit) where
  has = \case
    ExprTag_Arithmetic2 -> id
    ExprTag_Balance     -> id
    ExprTag_Lit         -> id
    ExprTag_Receive     -> id
    ExprTag_Send        -> id
    ExprTag_Var         -> id

instance UniverseSome (ExprTag Address) where
  universeSome = [Some ExprTag_Lit, Some ExprTag_Var]
instance UniverseSome (ExprTag [Address]) where
  universeSome = [Some ExprTag_Lit, Some ExprTag_Var]
instance UniverseSome (ExprTag Amount) where
  universeSome = [Some ExprTag_Lit, Some ExprTag_Balance, Some ExprTag_Receive, Some ExprTag_Send, Some ExprTag_Var, Some ExprTag_Arithmetic2]
instance UniverseSome (ExprTag Word) where
  universeSome = [Some ExprTag_Lit, Some ExprTag_Var]

instance UniverseSome (ExprTag lit) => FiniteSome (ExprTag lit)


-- Binary
type BinaryConstraints :: Type -> Type -> (Type -> Constraint) -> Constraint
type BinaryConstraints op a c = (c op, c a)

deriving stock    instance BinaryConstraints op a Eq       => Eq          (Binary op a)
deriving stock    instance BinaryConstraints op a Ord      => Ord         (Binary op a)
deriving stock    instance BinaryConstraints op a Show     => Show        (Binary op a)
deriving anyclass instance BinaryConstraints op a FromJSON => FromJSON    (Binary op a)
deriving anyclass instance BinaryConstraints op a ToJSON   => ToJSON      (Binary op a)
deriving anyclass instance BinaryConstraints op a FromJSON => FromJSONKey (Binary op a)
deriving anyclass instance BinaryConstraints op a ToJSON   => ToJSONKey   (Binary op a)

-- Transfer

type TransferConstraints :: Type -> (Type -> Type) -> (Type -> Constraint) -> Constraint
type TransferConstraints a p c = (c a, c (p Address))

deriving stock    instance TransferConstraints a p Eq       => Eq          (Transfer a p)
deriving stock    instance TransferConstraints a p Ord      => Ord         (Transfer a p)
deriving stock    instance TransferConstraints a p Show     => Show        (Transfer a p)
deriving anyclass instance TransferConstraints a p FromJSON => FromJSON    (Transfer a p)
deriving anyclass instance TransferConstraints a p ToJSON   => ToJSON      (Transfer a p)
deriving anyclass instance TransferConstraints a p FromJSON => FromJSONKey (Transfer a p)
deriving anyclass instance TransferConstraints a p ToJSON   => ToJSONKey   (Transfer a p)

-- Condition

type ConditionConstraints :: (Type -> Type) -> (Type -> Type) -> (Type -> Type) -> (Type -> Constraint) -> Constraint
type ConditionConstraints e p l c =
  ( c (Transfer (Rights e p l) p)
  , c (Issuance e p l)
  , c (l (Condition e p l))
  , c (Threshold, l (Condition e p l))
  , c Address
  , c (Uses, Condition e p l)
  , c (Duration, Bool, Condition e p l)
  , c (Binary Relation (e Amount))
  )

instance FiniteSome (ConditionTag e p l)

instance (ConditionConstraints e p l c) => Has c (ConditionTag e p l) where
  has = \case
    ConditionTag_All          -> id
    ConditionTag_Any          -> id
    ConditionTag_Counterparty -> id
    ConditionTag_Finite       -> id
    ConditionTag_Temporary    -> id
    ConditionTag_Restriction  -> id
    ConditionTag_Signature    -> id

-- Optics
instance Wrapped (Transfer a p) where
  type Unwrapped (Transfer a p) = (p Address, Role, a)
  _Wrapped' = iso u w
    where
    u (Transfer c r a) = (c, r, a)
    w (c, r, a) = Transfer c r a

instance Wrapped (Var a) where
  type Unwrapped (Var a) = (VariableTag a, Text)
  _Wrapped' = iso u w
    where
      u (Var t n) = (t, n)
      w (t, n) = (Var t n)

--TODO: better timestamp binding
instance Wrapped (Oracle a) where
  type Unwrapped (Oracle a) = (PublicKey, Var a, Timestamp)
  _Wrapped' = iso u w
    where
      u (Oracle a b c) = (a,b,c)
      w (a,b,c) = (Oracle a b c)

instance Wrapped (Binary op a) where
  type Unwrapped  (Binary op a) = (a, op, a)
  _Wrapped' = iso u w
    where
      u (Binary a b c) = (a,b,c)
      w (a,b,c) = (Binary a b c)

_List_Lit :: Prism' (List elem) [elem]
_List_Lit = newtypeDSumGEqIdentityPrism ListTag_Lit


_Condition_All :: Prism' (Condition e p l) (l (Condition e p l))
_Condition_All = dSumGEqIdentityPrism ConditionTag_All

_Condition_Any :: Prism' (Condition e p l) (Threshold, l (Condition e p l))
_Condition_Any = dSumGEqIdentityPrism ConditionTag_Any

_Condition_Counterparty :: Prism' (Condition e p l) PublicKey
_Condition_Counterparty = dSumGEqIdentityPrism ConditionTag_Counterparty

_Condition_Finite :: Prism' (Condition e p l) (Uses, Condition e p l)
_Condition_Finite = dSumGEqIdentityPrism ConditionTag_Finite

_Condition_Temporary :: Prism' (Condition e p l) (Duration, Bool, Condition e p l)
_Condition_Temporary = dSumGEqIdentityPrism ConditionTag_Temporary

_Condition_Restriction :: Prism' (Condition e p l) (Binary Relation (e Amount))
_Condition_Restriction = dSumGEqIdentityPrism ConditionTag_Restriction

_Condition_Signature :: Prism' (Condition e p l) PublicKey
_Condition_Signature = dSumGEqIdentityPrism ConditionTag_Signature




-- Serialisation
arithmeticType :: Binding Arithmetic2
arithmeticType = Binding_Defined "Arithmetic"

arithmetic_ :: Named (Enumeration Arithmetic2)
arithmetic_ = named "Arithmetic" $ enumeration $ \case
  Arithmetic2_Add -> "Add"
  Arithmetic2_Div -> "Div"
  Arithmetic2_Mul -> "Mul"
  Arithmetic2_Sub -> "Sub"

instance ToJSON Arithmetic2 where toJSON = toJSONEnumeration arithmetic_
instance FromJSON Arithmetic2 where parseJSON = parseJSONEnumeration arithmetic_

relationType :: Binding Relation
relationType = Binding_Defined "Relation"

relation_ :: Named (Enumeration Relation)
relation_ = named "Relation" $ enumeration $ \case
  Relation_EQ -> "EQ"
  Relation_LT -> "LT"
  Relation_LE -> "LE"
  Relation_GT -> "GT"
  Relation_GE -> "GE"

instance ToJSON Relation where toJSON = toJSONEnumeration relation_
instance FromJSON Relation where parseJSON = parseJSONEnumeration relation_

tokenType :: Binding Token
tokenType = Binding_Defined "Token"

token_ :: Named (Enumeration Token)
token_ = named "Token" $ enumeration $ \case
  BTC  -> "BTC"
  ETH  -> "ETH"
  USDC -> "USDC"
  USDT -> "USDT"
  SALT -> "SALT"

variableTagType :: Binding (VariableTag a)
variableTagType = Binding_Defined "VariableTag"

variableTag_ :: Named (Enumeration (Some VariableTag))
variableTag_ = named "VariableTag" $ enumeration $ \case
  Some VariableTag_Address -> "Address"
  Some VariableTag_Amount  -> "Amount"
  Some VariableTag_Count   -> "Count"
  Some VariableTag_List    -> "List"

instance FromJSON (Some VariableTag) where parseJSON = parseJSONEnumeration variableTag_
instance ToJSON   (Some VariableTag) where toJSON = toJSONEnumeration variableTag_

instance ToJSON   (VariableTag a) where toJSON = toJSON . Some


variableType :: Binding (Var a)
variableType = Binding_Defined "Variable"

variable_ :: FromJSON (VariableTag a) => Named (Struct (Var a))
variable_ = named "Variable" $ isomap _Unwrapped' $ pair
  (field "kind" variableTagType)
  (field "name" strType)

instance FromJSON (VariableTag a) => FromJSON (Var a) where parseJSON = parseJSONStruct variable_
instance FromJSON (VariableTag a) => ToJSON (Var a) where toJSON = toJSONStruct variable_


intentType :: Binding (Condition Expr Expr [])
intentType = Binding_Defined "Intent"

exprType :: Binding (Expr a)
exprType = Binding_Defined "Expr"

instance FromJSON (Condition Expr Expr []) where parseJSON = parseJSONUnion intent_
instance ToJSON (Condition Expr Expr []) where toJSON = toJSONUnion intent_
instance FromJSON (Expr Amount) where parseJSON = fmap NewtypeDSum . parseJSONUnion (expr_ float64Type)
instance ToJSON (Expr Amount) where toJSON = toJSONUnion (expr_ float64Type) . unNewtypeDSum
instance FromJSON (Expr Address) where parseJSON = fmap NewtypeDSum . parseJSONUnion (expr_ publicKeyType)
instance ToJSON (Expr Address) where toJSON = toJSONUnion (expr_ publicKeyType) . unNewtypeDSum

intent_ :: Named (Union (ConditionTag Expr Expr []))
intent_ = named "Intent" $ union $ \case
  ConditionTag_All -> named "All" $
    field "children" $ listType intentType

  ConditionTag_Any -> named "Any" $ pair
    (field "threshold" $ word64Type)
    (field "children"  $ listType intentType)

  ConditionTag_Counterparty -> named "Counterparty" $
    field "address" publicKeyType

  ConditionTag_Signature -> named "Signature" $
    field "signer" publicKeyType

  ConditionTag_Restriction -> named "Restriction" $ binary_ "relation" relationType exprType

  ConditionTag_Finite      -> named "Finite" $ pair
    (field "uses" word64Type)
    (field "inner" intentType)

  ConditionTag_Temporary   -> named "Temporary" $ triple
    (field "duration" timeType)
    (field "availableAfter" boolType)
    (field "inner" intentType)

expr_
  :: forall lit
   . ( FromJSON lit, ToJSON lit
     , FromJSON (Expr lit), ToJSON (Expr lit)
     , FromJSON (Var lit), ToJSON (Var lit)
     ) --TODO: simplify
  => Binding lit -> Named (Union (ExprTag lit))
expr_ litType = named "Expr" $ union $ \case
  ExprTag_Lit     -> named "Lit" $ field "value" $ litType
  ExprTag_Balance -> named "Balance" $ field "token" $ tokenType
  ExprTag_Send    -> named "Send" $ field "token"  $ tokenType
  ExprTag_Receive -> named "Receive" $ field "token"  $ tokenType
  ExprTag_Var    -> named "Var" $ field "var" $ variableType
  ExprTag_Arithmetic2 -> named "Arithmetic2" $ binary_ "operation" arithmeticType exprType

binary_
  :: (FromJSON a, ToJSON a, FromJSON op, ToJSON op)
  => Text -> Binding op -> Binding a -> Struct (Binary op a)
binary_ opName opType argType = isomap _Unwrapped' $ triple
  (field "lhs" argType)
  (field opName opType)
  (field "rhs" argType)

-- Utils
-- TODO: collisions?
holderVariableName :: Identifier
holderVariableName = "holder"

flipRole :: Role -> Role
flipRole = \case
  Role_Source -> Role_Sink
  Role_Sink -> Role_Source

insertName :: Var a -> VariableNames -> VariableNames
insertName (Var tg name) = DMap.insertWith (<>) tg $ Const $ Set.singleton name
insertValue :: Var a -> a -> VariableValues -> VariableValues
insertValue (Var tg name) a = DMap.insertWith (<>) tg $ Compose $ Map.singleton name $ Identity a

flattenAll :: Condition Expr Expr [] -> Condition Expr Expr []
flattenAll c = case c of
  Condition_All xs -> Condition_All $ do
    xs >>= \case
      Condition_All ys -> fmap flattenAll ys
      _ -> pure c
  _ -> c


swapPolicy :: Maybe Address -> AssetF Expr Map -> AssetF Expr Map -> Condition Expr Expr []
swapPolicy them receiving sending = flattenAll $ Condition_All
  [ receivingFunds them receiving
  , sendingFunds   them sending
  ]

-- Sending/receiving from the perspective of <intent> owner
sendingFunds, receivingFunds :: Maybe Address -> AssetF Expr Map -> Condition Expr Expr []
(sendingFunds, receivingFunds) = (restrictions Expr_Sending, restrictions Expr_Receiving)
  where
    restrictions direction counterparty assets = flattenAll $ Condition_All $ flip fmap (Map.toList assets) $ \(t, a) ->
      Condition_All $ catMaybes
        [ fmap Condition_Counterparty counterparty
        , Just $ Condition_Restriction $ Binary (direction t) Relation_EQ a
        ]

evalOp :: Fractional a => Arithmetic2 -> a -> a -> a
evalOp = \case
  Arithmetic2_Add -> (+)
  Arithmetic2_Div -> (/)
  Arithmetic2_Mul -> (*)
  Arithmetic2_Sub -> (-)
