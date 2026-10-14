{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Saline.App.Storage (Entry(..), Storage, get, local, nuke, remove, set, withStorage) where

import Prelude hiding (id, (.))

import Control.Category ({-id,-} (.))
import Control.Categorical.Bifunctor
import Control.Monad (join)
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Crypto.Hash (Digest, digestFromByteString)
import Crypto.Hash.IO (HashAlgorithm (HashDigestSize))
import Data.ByteArray (ByteArray, ByteArrayAccess)
import Data.ByteArray qualified as ByteArray
import Data.ByteArray.Sized (SizedByteArray (unSizedByteArray), sizedByteArray)
import Data.ByteArray.Sized qualified as Sized
import Data.ByteString
import Data.Functor.Identity
import Data.Maybe (fromJust)
import Data.Proxy
import Data.Some
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.TypeNats (KnownNat, natVal, type (+), type (<=))
import GHCJS.DOM qualified as DOM
import GHCJS.DOM.Storage qualified as Storage
import GHCJS.DOM.Window qualified as Window
import Language.Javascript.JSaddle (MonadJSM)
import Obelisk.Route
import Obelisk.Route.TH
import Control.Monad.Fix
import Data.Dependent.Map (DMap)
import Data.Dependent.Map qualified as DMap
import Data.Foldable
import Data.Functor.Compose
import Data.Functor.Const (Const (..))
import Data.Semigroup (First (..))
import Data.Universe.Some (UniverseSome (..))
import Obelisk.Route.Frontend
import Reflex.Dom.Core

import Saline.Crypto (Checksum, EncryptedSecret, Salt, hexEncoder)
import Saline.Utils
import Saline.Utils.Uniques (Uniques')
import Saline.Utils.Uniques qualified as Uniques
import Saline.App.Utils

data Storage = Storage
  { _storage_encodeKey :: Encoder Identity (Either Text) (Some Entry) Text
  , _storage_encodeValue :: forall x. Entry x -> Encoder Identity (Either Text) x Text
  , _storage_underlying :: forall m. MonadJSM m => m Storage.Storage
  }

data Entry v where
  Entry_Secret  :: Entry (EncryptedSecret, (Salt, Checksum))
  Entry_Wallets :: Entry (Uniques' [] Text)

fmap join $ traverse deriveRouteComponent
  [ ''Entry
  ]

nuke :: MonadJSM m => Storage -> m ()
nuke s = for_ universeSome $ foldSome $ remove s

withStorage
  :: (DomBuilder t n, MonadJSM n, MonadHold t n, MonadFix n)
  => (Storage -> DMap Entry (Compose (Dynamic t) Maybe) -> RoutedT t r (EventWriterT t (DMap Entry Identity) n) ())
  -> RoutedT t r n ()
withStorage f = do
  local >>= \case
    Left err -> copoutTODO err --TODO: try to make this impossible
    Right s -> do
      let
        --TODO: what to do if storage is corrupt? allow local widgets to gracefully handle or bail and enter some recovery mode?
        corruptStorage = copoutTODO

        keys = foldMap (\(Some k) -> DMap.singleton k (Const ())) universeSome

      snapshot <- getCompose $ DMap.forWithKey keys $ \k _ ->
        Compose $ get s k

      case snapshot of
        Left err -> corruptStorage err
        Right read0 -> do
          replaceSelf_ (Right ()) $ \case
            Left err -> never <$ corruptStorage err
            Right () -> mdo
              --TODO: should this be an Event of DSum instead?
              ((), ev) <- mapRoutedT runEventWriterT $ f s d

              let sel = fan ev
              (d, errEv) <- runEventWriterT $ DMap.forWithKey read0 $ \k v0 -> do
                let writeEv = select sel k
                ((), readEv) <- runWithReplace blank $ ffor writeEv $ \v -> do
                  set s k v
                  get s k
                let (err, ok) = fanEither readEv
                tellEvent $ First . Left <$> err
                fmap Compose $ holdDyn v0 ok

              pure $ fmap getFirst errEv

local :: MonadJSM m => m (Either Text Storage)
local = runExceptT $ do
  w <- maybe (throwError "Could not retrieve DOM.currentWindow") pure =<< DOM.currentWindow
  k <- either throwError pure $ checkEncoder keyEncoder
  pure $ Storage k valueEncoder $ Window.getLocalStorage w

remove :: MonadJSM m => Storage -> Entry a -> m ()
remove s k = withEntry s k $ \s' k' _ ->
  Storage.removeItem s' k'

set :: MonadJSM m => Storage -> Entry a -> a -> m ()
set s k v = withEntry s k $ \s' k' e ->
  Storage.setItem s' k' $ encode e v

get :: MonadJSM m => Storage -> Entry a -> m (Either Text (Maybe a))
get s k = withEntry s k $ \s' k' e ->
  ffor (Storage.getItem s' k') $
    traverse $ tryDecode e

withEntry
  :: MonadJSM m
  => Storage
  -> Entry a
  -> (Storage.Storage -> Text -> Encoder Identity (Either Text) a Text -> m b)
  -> m b
withEntry s k f = do
  s' <- _storage_underlying s
  f
    s'
    (encode (_storage_encodeKey s) (Some k))
    (_storage_encodeValue s k)

keyEncoder :: (MonadError Text check, MonadError Text parse) => Encoder check parse (Some Entry) Text
keyEncoder = enum1Encoder $ \case
  Entry_Secret -> "secret"
  Entry_Wallets -> "wallets"

valueEncoder :: MonadError Text parse => Entry a -> Encoder Identity parse a Text
valueEncoder = \case
  Entry_Secret -> hexEncoder False . unsizedByteArrayEncoder . appendEncoder . second (appendEncoder . second digestToSizedByteArrayEncoder)
  Entry_Wallets -> jsonEncoder . reviewEncoder (Uniques.prism Uniques.list)

-- TODO: try instead adding upstream `instance (ByteArrayN (HashDigestSize ha)) Digest`
digestToSizedByteArrayEncoder
  :: (Applicative check, Applicative parse, HashAlgorithm ha, KnownNat (HashDigestSize ha))
  => Encoder check parse (Digest ha) (SizedByteArray (HashDigestSize ha) ByteString)
digestToSizedByteArrayEncoder = unsafeMkEncoder $ EncoderImpl
  { _encoderImpl_encode = fromJust . sizedByteArray . ByteArray.convert
  , _encoderImpl_decode = pure . fromJust . digestFromByteString . unSizedByteArray
  }

appendEncoder
  :: forall n m ba check parse
   . ( Applicative check, MonadError Text parse, ByteArray ba
     , KnownNat n, KnownNat m, KnownNat (m+n)
     , n <= m + n
     )
  => Encoder check parse (SizedByteArray n ba, SizedByteArray m ba) (SizedByteArray (m + n) ba)
appendEncoder = unsafeMkEncoder $ EncoderImpl
  { _encoderImpl_encode = uncurry Sized.append
  , _encoderImpl_decode = pure . Sized.splitAt
  }

unsizedByteArrayEncoder
  :: forall check parse ba n.
     ( Applicative check, MonadError Text parse
     , KnownNat n
     , ByteArrayAccess ba
     )
  => Encoder check parse (SizedByteArray n ba) ba
unsizedByteArrayEncoder = unsafeMkEncoder $ EncoderImpl
  { _encoderImpl_encode = unSizedByteArray
  , _encoderImpl_decode = \ba -> maybe (bail ba) pure $ sizedByteArray ba
  }
  where
    bail ba = throwError $ Text.intercalate " "
      [ "unsizedByteArrayEncoder: invalid size."
      , "Expected"
      , tshow (natVal $ Proxy @n)
      , "but got"
      , tshow $ ByteArray.length ba
      ]
