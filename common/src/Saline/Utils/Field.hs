{-# LANGUAGE DerivingVia #-}
module Saline.Utils.Field where

import Control.Monad.Trans.Writer
import Data.Bitraversable
import Data.Functor.Compose
import Data.Monoid
import Data.Semialign
import Data.Text
import Data.These (These)
import Reflex.Dom.Core (ffor2)

import Saline.Utils (treadEither)

data Hint = Hint_Warning
  deriving stock (Eq, Ord, Read, Show)

-- TODO: make layers toggle-able
newtype Field e a = Field { unField :: Either e (Maybe (Writer (Alt Maybe Hint) a)) }
  deriving stock (Eq, Ord, Read, Show, Functor)
  deriving Applicative via Compose (Either e) (Compose Maybe (Writer (Alt Maybe Hint)))

maybeField :: Maybe a -> Field e a
maybeField = maybe unfilled pure

eitherField :: Either e a -> Field e a
eitherField = Field . fmap (pure . pure)

unfilled :: Field e a
unfilled = Field $ Right Nothing

isUnfilled :: Field e a -> Bool
isUnfilled = \case
  Field (Right Nothing) -> True
  _ -> False

justFieldValue :: Field e a -> Maybe a
justFieldValue = \case
  Field (Right input) -> fst . runWriter <$> input
  _ -> Nothing

these :: Field e a -> Field e b -> Field e (These a b)
these (Field fa) (Field fb) = Field $ ffor2 fa fb $ alignWith bisequence

treadField :: Read a => Text -> Field Text a
treadField = Field . fmap (pure . pure) . treadEither
