{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Saline.Network.Node.Storage where

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Map (Map)
import Data.Time
import GHC.Generics (Generic)

import Saline.Crypto (PublicKey)
import Saline.Intents
import Saline.Intents.Condition

data Store = Store
  { _store_balances  :: Map PublicKey Balances
  , _store_intents   :: Map PublicKey Intent
  , _store_oracles   :: Map PublicKey VariableValues
  , _store_timestamp :: DiffTime
  , _store_contracts :: Contracts (Map (PublicKey, PublicKey) Word) -- (issuer, holder)
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, FromJSONKey, ToJSON, ToJSONKey)

makeLenses ''Store
