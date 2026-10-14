{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module Saline.Network.Node.RPC where

import Control.Error.Util (hoistEither)
import Control.Monad.Except (MonadError(throwError), ExceptT(ExceptT), runExceptT, withExceptT)
import Control.Monad.IO.Class
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteArray.HexString (HexString(..), unHexString)
import Data.ByteArray.HexString qualified as HexString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Language.Javascript.JSaddle
import Network.Tendermint.Client qualified as Client
import Obelisk.Route (R, encode, pattern (:/))
import System.Console.ANSI
import Text.URI (URI)
import Text.URI qualified as URI

import Saline.Interface.Query
import Saline.Network.Node.Transaction
import Saline.Utils

#ifdef ghcjs_HOST_OS
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Lens (view, lazy, (^.), Getting)
import Data.Aeson (FromJSON, (.:))
import Data.ByteArray.Base64String (Base64String (unBase64String))
import Data.Functor (void)
import Saline.Crypto (bytestring2hex)
#else
import Control.Exception (SomeException, try)
import Control.Lens (view, lazy)
import Control.Monad (unless, when)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Base16.Types qualified as Base16
import Data.Bifunctor
import Data.ByteArray.Base64String (Base64String (Base64String, unBase64String))
import Data.ByteArray.Base64String qualified as Base64
import Data.ByteString.Base16 qualified as Base16
import Data.Default (Default (def))
import Data.Int (Int64)
import Network.ABCI.Types.Messages.FieldTypes (WrappedVal (WrappedVal))
import Network.ABCI.Types.Messages.Response qualified as Response
import Network.Tendermint.Client (RequestABCIQuery (..), ResultABCIQuery (..))
#endif

rpcEnv :: ThreadEnv
rpcEnv = ThreadEnv
  { _threadEnv_component = "RPC"
  , _threadEnv_color = Cyan
  }

type RPC a = URI -> IO (Either Text a)

-- TODO: move somewhere else
callbackFromJS :: (Aeson.Value -> Aeson.Parser a) -> (a -> JSM ()) -> JSCallAsFunction
callbackFromJS parse f = fun $ \_ _ -> \case
  [arg] -> fromJSVal arg >>= \case
    Nothing -> pure () --TODO: what now?
    Just x -> case Aeson.parseEither parse x of
      Left _msg -> liftIO $ print $ "ERR: " <> _msg --TODO: what now?
      Right a -> f a
  _ -> pure () --TODO: what now?


#ifdef ghcjs_HOST_OS
rpc
  :: (MonadJSM m, MakeObject mo)
  => (Aeson.Value -> Aeson.Parser Text)
  -> (Aeson.Value -> Aeson.Parser a)
  -> Getting mo (JSM JSVal) mo
  -> m (Either Text a)
rpc parseCatch parseThen invocation = awaitPromise parseCatch parseThen
  $ jsg ("window" :: Text)
  ^. js ("saline" :: Text)
  ^. js ("rpc" :: Text)
  ^. invocation

parseRpcError :: Aeson.Value -> Aeson.Parser Text
parseRpcError = parseField "data" "Error"

parseField :: Aeson.FromJSON a => Aeson.Key -> String -> Aeson.Value -> Aeson.Parser a
parseField key = flip Aeson.withObject $ \o -> o .: key

parseQueryValue :: Aeson.Value -> Aeson.Parser Base64String
parseQueryValue = Aeson.withObject "QueryResponse" $ \o -> do
  r <- o .: "response"
  r .: "value"

parseBroadcastData :: Aeson.Value -> Aeson.Parser Base64String
parseBroadcastData = Aeson.withObject "BroadcastResponse" $ \o -> do
  r <- o .: "result"
  r .: "value"

awaitPromise
  :: (MonadJSM m, MakeObject mo)
  => (Aeson.Value -> Aeson.Parser Text)
  -> (Aeson.Value -> Aeson.Parser a)
  -> mo
  -> m (Either Text a)
awaitPromise parseCatch parseThen promise = do
  v <- liftIO newEmptyMVar

  void $ liftJSM $ promise ^. js2 ("then" :: Text)
    (callbackFromJS parseThen  $ liftIO . putMVar v . Right)
    (callbackFromJS parseCatch $ liftIO . putMVar v . Left)

  liftIO $ takeMVar v

#else
broadcastTxSyncM :: Base64String -> Client.TendermintM Client.ResultBroadcastTx
broadcastTxSyncM = Client.broadcastTxSync . Client.RequestBroadcastTxSync

healthM :: Client.TendermintM Client.ResultHealth
healthM = Client.health

abciInfoM :: Client.TendermintM Client.ResultABCIInfo
abciInfoM = Client.abciInfo

blockM :: Maybe Int64 -> Client.TendermintM Client.ResultBlock
blockM = Client.block . Client.RequestBlock . fmap WrappedVal

transactionM :: Base64String -> Client.TendermintM Client.ResultTx
transactionM hash = Client.tx $ Client.RequestTx
  { requestTxHash = Just hash
  , requestTxProve = False
  }

rpc :: Text -> Maybe Text -> Client.TendermintM a -> RPC a
rpc endpoint message req uri = fmap (first tshow) $ do
  let
    (host, port) = unsafeHostPortFromURI uri
    cfg = Client.defaultConfig (Text.encodeUtf8 host) (fromEnum port) True

  when False $
    flip runReaderT rpcEnv $ logger $ URI.render uri <> "/" <> endpoint <> maybe "" (" \t" <>) message

  r <- try @SomeException $ Client.runTendermintM cfg req

  case r of
    Left err -> print err
    Right _ -> pure ()

  pure r
#endif

health :: RPC Client.ResultHealth
#ifdef ghcjs_HOST_OS
health uri = rpc parseRpcError Aeson.parseJSON $ js1 ("health" :: Text) (URI.render uri)
#else
health = rpc "health" Nothing healthM
#endif

abciInfo :: RPC Client.ResultABCIInfo
#ifdef ghcjs_HOST_OS
abciInfo uri = rpc parseRpcError Aeson.parseJSON $ js1 ("abci_info" :: Text) (URI.render uri)
#else
abciInfo = rpc "abci_info" Nothing abciInfoM
#endif

block :: Maybe Height -> RPC Client.ResultBlock
#ifdef ghcjs_HOST_OS
block height uri = rpc parseRpcError Aeson.parseJSON $ js2 ("block" :: Text) (URI.render uri) height
#else
block height = rpc "block" (fmap tshow height) (blockM $ fmap fromIntegral height)
#endif

transaction :: Hash SignedTx -> RPC Client.ResultTx
#ifdef ghcjs_HOST_OS
transaction (Hash_Tx bs) uri = rpc parseRpcError Aeson.parseJSON $ js2 ("transaction" :: Text) (URI.render uri) (bytestring2hex bs)
#else
transaction h@(Hash_Tx bs) = rpc "tx" (Just $ renderHash h) $ transactionM $ Base64String bs
#endif

#ifdef ghcjs_HOST_OS
query :: FromJSON b => SalineEncoder -> (R f -> R CometQuery) -> f (Req a b) -> a -> RPC b
query enc mkQuery k v uri = runExceptT $ do
  let
    -- !!!!!GHCJS BUG!!!!!
    -- Not evaluating `path` here somehow causes a thunk to be serialized into some highly nested JSON with d1/d2 field names
    -- Similar to https://github.com/ghcjs/ghcjs/issues/307
    (!path, dat) = encode enc $ mkQuery (k :/ Req v) -- we need to receive this in pieces to know `b`

  resp <- ExceptT $ rpc parseRpcError parseQueryValue $ js3 ("query" :: Text) (URI.render uri) path (bytestring2hex dat)
  withExceptT Text.pack $ hoistEither $ Aeson.eitherDecode' $ view lazy $ unBase64String resp
#else
query :: (Aeson.FromJSON b, Show b) => SalineEncoder -> (R f -> R CometQuery) -> f (Req a b) -> a -> RPC b
query enc mkQuery k v uri = runExceptT $ do
  let
    (path, dat) = encode enc $ mkQuery (k :/ Req v) -- we need to receive this in pieces to know `b`

    q = def
      { requestABCIQueryData = HexString $ Base16.extractBase16 $ Base16.encodeBase16' dat
      , requestABCIQueryPath = Just path
      }

  ResultABCIQuery resp <- ExceptT $ rpc "query" (requestABCIQueryPath q) (Client.abciQuery q) uri
  output <- withExceptT Text.pack $ hoistEither $ Aeson.eitherDecode' $ view lazy $ unBase64String $ Response.queryValue resp
  unless (Response.queryCode resp == 0) $ throwError $ tshow output
  pure output
#endif

broadcastTxSync :: SignedTx -> RPC (Hash SignedTx)
broadcastTxSync stx uri = runExceptT $ do
  let bytes = serializeTx stx
#ifdef ghcjs_HOST_OS
  res <- ExceptT $ rpc parseRpcError Aeson.parseJSON $ js2 ("broadcastTxSync" :: Text) (URI.render uri) (bytestring2hex bytes)
#else
  let toBase64 = Base64.fromBytes . Base16.extractBase16 . Base16.encodeBase16'
  res <- ExceptT $ rpc "broadcast_tx_sync" (Just $ tshow (_signed_nonce stx, _signed_signee stx)) (broadcastTxSyncM $ toBase64 bytes) uri
#endif

  let
    txHash = Client.resultBroadcastTxHash res
    hex = HexString.format txHash

  case Client.resultBroadcastTxCode res of
    0 -> pure $ Hash_Tx $ unHexString txHash
    _ -> throwError $ case Text.decodeUtf8' $ unHexString $ Client.resultBroadcastTxData res of
      Left e -> hex <> " error when decoding result data: " <> tshow e
      Right b -> b
