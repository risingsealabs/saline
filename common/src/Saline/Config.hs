{-# LANGUAGE OverloadedStrings #-}
module Saline.Config where

import Control.Error (note)
import Control.Exception (Exception(displayException))
import Control.Monad ((<=<))
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Encoding (decodeUtf8')
import Obelisk.Configs (HasConfigs(getConfig))
import Obelisk.Route (tryDecode)
import Reflex.Dom.Core
import Text.URI (mkURI, URI)

import Saline.Crypto

type WithConfig a b = (Text -> b) -> (a -> b) -> b

withNodesConfig :: HasConfigs m => WithConfig (NonEmpty URI) (m ())
withNodesConfig = withConfigFromDir "frontend/node" (parseLinesNonEmpty parseURL)

withRouteConfig :: HasConfigs m => WithConfig URI (m ())
withRouteConfig = withConfigFromDir "common/route" parseURL

withWatcherConfig :: Text -> Maybe ByteString -> WithConfig PublicKey (m ())
withWatcherConfig path fileContents = withConfig path fileContents (tryDecode publicKeyEncoder)

parseLines :: (Text -> Either Text a) -> (Text -> Either Text [a])
parseLines f = traverse f . Text.lines

parseLinesNonEmpty :: (Text -> Either Text a) -> (Text -> Either Text (NonEmpty a))
parseLinesNonEmpty f = note "parseLinesNonEmpty: empty list" . nonEmpty <=< parseLines f

parseURL :: Text -> Either Text URI
parseURL = first (Text.pack . displayException) . mkURI . Text.strip

getConfigFeedbackDOM :: DomBuilder t m => Text -> m ()
getConfigFeedbackDOM = text

getConfigFeedbackLogs :: MonadIO m => Text -> m ()
getConfigFeedbackLogs = liftIO . Text.putStrLn

withConfigFromDir :: HasConfigs m => Text -> (Text -> Either Text a) -> WithConfig a (m ())
withConfigFromDir path parse onErr onSuccess = do
  contents <- getConfig path
  withConfig path contents parse onErr onSuccess

withConfig :: Text -> Maybe ByteString -> (Text -> Either Text a) -> WithConfig a b
withConfig path contents parse onErr onSuccess = do
  let quoted x = "'" <> x <> "'"
  case contents of
    Nothing -> onErr $ "No config file found in " <> quoted path
    Just bytes -> case decodeUtf8' bytes of
      Left err -> onErr $ "Couldn't decode config " <> quoted path <> " : " <> Text.pack (displayException err)
      Right txt -> case parse txt of
        Left err -> onErr $ Text.intercalate " "
          [ "Couldn't parse config"
          , quoted path
          , ":"
          , quoted txt
          , ":"
          , err
          ]
        Right a -> onSuccess a
