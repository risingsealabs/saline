{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Saline.Integrations.Aeson where

import Prelude hiding (lines, unlines, writeFile)

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative(liftA2)
#endif
import Control.Applicative (Alternative (empty))
import Control.Monad
import Data.Aeson (KeyValue ((.=)), (.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Dependent.Sum (DSum ((:=>)))
import Data.Foldable (asum)
import Data.Functor.Identity
import Data.Functor.Invariant
import Data.Some
import Data.String (IsString (fromString))
import Data.Text qualified as Text
import Data.Universe (Finite (..))
import Data.Universe.Some

import Saline.Integrations.Bindings
import Saline.Utils (Monoidal (..))

data Aeson a = Aeson
  { _aeson_toEncoding :: a -> Aeson.Series --TODO: dedup?
  , _aeson_toJSON :: a -> [Aeson.Pair] --TODO: cannot control order!
  , _aeson_fromJSON :: Aeson.Object -> Aeson.Parser a
  }

instance Invariant Aeson where
  invmap :: (a -> b) -> (b -> a) -> Aeson a -> Aeson b
  invmap fw bw a = Aeson
    { _aeson_toEncoding = _aeson_toEncoding a . bw
    , _aeson_toJSON = _aeson_toJSON a . bw
    , _aeson_fromJSON = fmap fw . _aeson_fromJSON a
    }

instance Monoidal Aeson where
  unit :: Aeson ()
  unit = Aeson
    { _aeson_toEncoding = \_ -> mempty
    , _aeson_toJSON = \_ -> mempty
    , _aeson_fromJSON = \_ -> pure ()
    }

  pair :: Aeson x -> Aeson y -> Aeson (x, y)
  pair fa fb = Aeson
    { _aeson_toEncoding = \(a,b) -> _aeson_toEncoding fa a <> _aeson_toEncoding fb b
    , _aeson_toJSON = \(a,b) -> _aeson_toJSON fa a <> _aeson_toJSON fb b
    , _aeson_fromJSON = \o -> liftA2 (,) (_aeson_fromJSON fa o) (_aeson_fromJSON fb o)
    }


toJSONEnumeration :: Named (Enumeration a) -> a -> Aeson.Value
toJSONEnumeration (Named _ e) = Aeson.String . _enumeration_caseName e

parseJSONEnumeration :: Named (Enumeration a) -> Aeson.Value -> Aeson.Parser a
parseJSONEnumeration (Named n (Enumeration caseName)) = Aeson.withText (Text.unpack n) $ \txt ->
  asum $ flip fmap universeF $ \a -> do
    guard $ caseName a == txt
    pure a


toJSONUnion :: Named (Union f) -> DSum f Identity -> Aeson.Value
toJSONUnion (Named _ u) (k :=> Identity v) = Aeson.object $ name : rest
  where
    Named n s = _union_structs u k
    name = "tag" .= n
    rest = _aeson_toJSON (evalStruct evalAeson s) v

toEncodingUnion :: Named (Union f) -> DSum f Identity -> Aeson.Encoding
toEncodingUnion (Named _ u) (k :=> Identity v) = Aeson.pairs (name <> rest)
  where
    Named n s = _union_structs u k
    name = "tag" .= n
    rest = _aeson_toEncoding (evalStruct evalAeson s) v

parseJSONUnion :: FiniteSome f => Named (Union f) -> Aeson.Value -> Aeson.Parser (DSum f Identity)
parseJSONUnion (Named typename u) = Aeson.withObject (Text.unpack typename) $ \o -> do
  tag <- o .: "tag"
  asum $ flip fmap universeFSome $ \(Some k) -> do
    let Named n s = _union_structs u k
    guard $ n == tag
    val <- _aeson_fromJSON (evalStruct evalAeson s) o
    pure $ k :=> Identity val


toJSONStruct :: Named (Struct a) -> a -> Aeson.Value
toJSONStruct (Named _ p) = Aeson.object . _aeson_toJSON (evalStruct evalAeson p)

parseJSONStruct :: Named (Struct a) -> Aeson.Value -> Aeson.Parser a
parseJSONStruct (Named n p) = Aeson.withObject (Text.unpack n) $ _aeson_fromJSON (evalStruct evalAeson p)

evalAeson :: StructEvaluator Aeson
evalAeson = StructEvaluator
  { _structEvaluator_field = \(Named name _) ->
    let k = fromString . Text.unpack $ name
    in Aeson
      { _aeson_toEncoding = (k .=)
      , _aeson_toJSON = pure . (k .=)
      , _aeson_fromJSON = (.: k)
      }
  , _structEvaluator_todo   = Aeson
      { _aeson_toEncoding = \_ -> mempty
      , _aeson_toJSON = \_ -> mempty
      , _aeson_fromJSON = \_ -> empty --TODO
      }
  }
