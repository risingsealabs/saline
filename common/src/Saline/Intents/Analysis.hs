module Saline.Intents.Analysis where

import Control.Monad.Trans.Writer
import Data.Functor.Identity
import Data.Monoid

import Saline.Intents.Condition
import Saline.Utils.Field

-- TODO: odd signature
analysis :: Condition Identity Identity List -> Field e (Condition Identity Identity List)
analysis p = Field $ pure $ pure $ writer (p, hints p)

hints :: Condition Identity Identity List -> Alt Maybe Hint
hints = \case
  Condition_All _      -> mempty
  Condition_Any 0 (List_Lit []) -> mempty
  Condition_Any 0 _    -> pure Hint_Warning
  Condition_Any k (List_Lit ps) -> case fromEnum k > length ps of
    True -> pure Hint_Warning
    False -> mempty
  Condition_Any _ _ -> mempty
  Condition_Counterparty _ -> mempty
  Condition_Finite _ _ -> mempty
  Condition_Temporary _ _ _ -> mempty
  Condition_Restriction _ -> mempty
  Condition_Signature _ -> mempty
