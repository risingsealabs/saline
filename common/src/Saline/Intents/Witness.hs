{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Saline.Intents.Witness where

import Data.Aeson
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Dependent.Sum
import Data.Functor.Identity
import Data.Map (Map)
import Data.Universe.Some
import Data.Universe.Some.TH
import Data.Word (Word64)

import Saline.Integrations.Aeson
import Saline.Integrations.Bindings
import Saline.Utils (AesonMap (..), Monoidal (..))
import Saline.Utils.TH

data WitnessTag a where
  WitnessTag_All :: WitnessTag [Witness]
  WitnessTag_Any :: WitnessTag (AesonMap Word64 Witness)
  WitnessTag_Auto :: WitnessTag ()

type Witness = DSum WitnessTag Identity

{-# COMPLETE Witness_All, Witness_Any, Witness_Auto #-}

pattern Witness_All :: [Witness] -> Witness
pattern Witness_All ws = WitnessTag_All :=> Identity ws

pattern Witness_Any :: Map Word64 Witness -> Witness
pattern Witness_Any ws = WitnessTag_Any :=> Identity (AesonMap ws)

pattern Witness_Auto :: Witness
pattern Witness_Auto = WitnessTag_Auto :=> Identity ()

deriveMany [id]
  [ deriveArgDict      ''WitnessTag
  , deriveStockGADT    ''WitnessTag
  , deriveUniverseSome ''WitnessTag
  ]

instance FiniteSome WitnessTag

witnessType :: Binding Witness
witnessType = Binding_Defined "Witness"

witness_ :: Named (Union WitnessTag)
witness_ = named "Witness" $ union $ \case
  WitnessTag_All  -> named "AllW" $ field "children" $ listType witnessType
  WitnessTag_Any  -> named "AnyW" $ field "children" $ dictType' word64Type witnessType
  WitnessTag_Auto -> named "AutoW" unit

instance FromJSON Witness where parseJSON = parseJSONUnion witness_
instance ToJSON   Witness where toJSON = toJSONUnion witness_
