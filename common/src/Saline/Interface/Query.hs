{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Saline.Interface.Query where

import Prelude hiding ((.))
import Control.Category ({-id,-} (.))
import Control.Monad (join)
import Control.Categorical.Bifunctor (first)
import Control.Category.Monoidal
import Control.Lens (iso)
import Control.Monad.Except (MonadError)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Functor.Identity
import Data.Kind (Type)
import Data.List.NonEmpty
import Data.Map (Map)
import Data.Ord
import Data.Set (Set)
import Data.Text
import GHC.Generics (Generic)
import Obelisk.Route hiding (queryOnlyEncoder)
import Obelisk.Route.TH

import Saline.Crypto (PublicKey)
import Saline.Intents (Intent, Wallet, Contracts)
import Saline.Intents.Condition (Amount, Token)
import Saline.Network.Node.Transaction

type SalineEncoder = CometQueryEncoder (R CometQuery)

type CometQueryEncoder r = Encoder Identity (Either Text) r (Text, ByteString)

-- https://docs.cometbft.com/v0.37/spec/abci/abci++_methods#query
data CometQuery :: Type -> Type where
  CometQuery_Store :: CometQuery (R SalineQuery)

data SalineQuery :: Type -> Type where
  SalineQuery_Intent    :: SalineQuery (Req PublicKey (Maybe Intent))
  SalineQuery_Intents   :: SalineQuery (Req () (Map Intent (Map PublicKey ())))
  SalineQuery_Contracts :: SalineQuery (Req () (Contracts (Map (PublicKey, PublicKey) Word)))
  SalineQuery_ForSale   :: SalineQuery (Req PublicKey (Contracts (Map PublicKey (Word, NonEmpty Tx))))
  SalineQuery_Rights    :: SalineQuery (Req PublicKey (Contracts (Map PublicKey Word)))
  SalineQuery_Duties    :: SalineQuery (Req PublicKey (Contracts (Map PublicKey Word)))
  SalineQuery_Balance   :: SalineQuery (Req (PublicKey, Token) Amount)
  SalineQuery_Wallet    :: SalineQuery (Req PublicKey Wallet)
  SalineQuery_Aggregate :: SalineQuery (Req (Set PublicKey) (Map Token Amount))
  SalineQuery_Swaps     :: SalineQuery (Req (PublicKey, (Token, Token)) (Map (Trade Amount) (NonEmpty Tx))) -- TODO: how to differentiate by counterparty/intent?

deriving stock instance Show (SalineQuery a)

newtype Trade a = Trade { unTrade :: (a, a) }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, FromJSONKey, ToJSON, ToJSONKey)

unitPrice :: Fractional a => Trade a -> a
unitPrice = uncurry (/) . unTrade

instance (Ord a, Fractional a) => Ord (Trade a) where
  compare = comparing unitPrice <> comparing unTrade


newtype Req a b = Req { unReq :: a }
  deriving stock (Eq, Ord, Show)

fmap join $ traverse deriveRouteComponent
  [ ''CometQuery
  , ''SalineQuery
  ]

salineQueryEncoder :: Encoder (Either Text) (Either Text) (R CometQuery) (Text, ByteString)
salineQueryEncoder = first (prefixTextEncoder "/" . pathSegmentsTextEncoder . listToNonEmptyEncoder) . pathEncoder
  where
    jsonReqEncoder :: (FromJSON a, ToJSON a, Applicative check, MonadError Text parse) => Encoder check parse (Req a b) ([Text], ByteString)
    jsonReqEncoder = queryOnlyEncoder . utf8Encoder . jsonEncoder . reqEncoder

    pathEncoder :: Encoder (Either Text) (Either Text) (R CometQuery) ([Text], ByteString)
    pathEncoder = pathComponentEncoder $ \case
      CometQuery_Store -> PathSegment "store" $ pathComponentEncoder $ \case
        SalineQuery_Intent    -> PathSegment "intent"       jsonReqEncoder
        SalineQuery_Intents   -> PathSegment "intents"      jsonReqEncoder
        SalineQuery_Contracts -> PathSegment "contracts"    jsonReqEncoder
        SalineQuery_ForSale   -> PathSegment "forSale"      jsonReqEncoder
        SalineQuery_Duties    -> PathSegment "duties"       jsonReqEncoder
        SalineQuery_Rights    -> PathSegment "rights"       jsonReqEncoder
        SalineQuery_Balance   -> PathSegment "balance"      jsonReqEncoder
        SalineQuery_Wallet    -> PathSegment "wallet"       jsonReqEncoder
        SalineQuery_Aggregate -> PathSegment "aggregate"    jsonReqEncoder
        SalineQuery_Swaps     -> PathSegment "swaps"        jsonReqEncoder

reqEncoder :: (Applicative check, Applicative parse) => Encoder check parse (Req a b) a
reqEncoder = viewEncoder $ iso unReq Req

-- Same impl as Obelisk.Route.queryOnlyEncoder but with generalised type
-- TODO: upstream?
queryOnlyEncoder :: (Applicative check, MonadError Text parse) => Encoder check parse a ([Text], a)
queryOnlyEncoder = first (unitEncoder []) . coidl
