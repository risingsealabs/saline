{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Saline.Network.CometBFT.Config where

import Control.Arrow (left, (>>>))
import Control.Lens (makeLenses)
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import GHC.Generics
import Text.URI (URI, mkURI, render)
import Toml (AnyValue, BiMap (..), TomlBiMap, TomlBiMapError (ArbitraryError), TomlCodec, match, (.=), _Text)
import Toml qualified

import Saline.Utils

data Config = Config
  { _config_moniker :: Text
  , _config_proxyApp :: URI
  , _config_fastSync :: Bool
  , _config_genesisFile :: Text
  , _config_rpc :: ConfigRPC
  , _config_p2p :: ConfigP2P
  , _config_consensus :: ConfigConsensus
  , _config_instrumentation :: ConfigInstr
  } deriving stock (Eq, Ord, Show, Generic)

data ConfigRPC = ConfigRPC
  { _configRPC_laddr :: URI
  , _configRPC_allowedOrigins :: [Text]

  } deriving stock (Eq, Ord, Show, Generic)

data ConfigP2P = ConfigP2P
  { _configP2P_laddr :: URI
  , _configP2P_persistentPeers :: Text
  , _configP2P_privatePeerIds :: Text
  , _configP2P_addrBookStrict :: Bool
  , _configP2P_allowDuplicateIp :: Bool
  } deriving stock (Eq, Ord, Show, Generic)

data ConfigConsensus = ConfigConsensus
  { _configConsensus_createEmptyBlocksInterval :: Text
  } deriving stock (Eq, Ord, Read, Show, Generic)

data ConfigInstr = ConfigInstr
  { _configInstr_prometheus :: Bool
  , _configInstr_prometheus_listen_addr :: Text
  , _configInstr_max_open_connections :: Word
  } deriving stock (Eq, Ord, Read, Show, Generic)

storeConfigFile :: MonadIO m => Text -> Config -> m ()
storeConfigFile configPath = liftIO . Text.writeFile (Text.unpack configPath) . Toml.encode configCodec

loadConfigFile :: MonadIO m => Text -> m Config
loadConfigFile configPath = liftIO $ do
  toml <- Text.readFile $ Text.unpack configPath
  case Toml.decode configCodec toml of
    Left err -> do
      print toml
      print err
      error "Failed decoding"
    Right config -> pure config

uri :: Toml.Key -> TomlCodec URI
uri = match _URI

_URI :: TomlBiMap URI AnyValue
_URI = _URI2Text >>> _Text

_URI2Text :: TomlBiMap URI Text
_URI2Text = BiMap
    { forward  = Right . removeTrailingSlash . render
    , backward = left (ArbitraryError . tshow) . mkURI
    }
  where
    removeTrailingSlash = Text.dropWhileEnd (== '/')

configCodec :: TomlCodec Config
configCodec = pure Config
  <*> Toml.text                 "moniker"                      .= _config_moniker
  <*> uri                       "proxy_app"                    .= _config_proxyApp
  <*> Toml.bool                 "fast_sync"                    .= _config_fastSync
  <*> Toml.text                 "genesis_file"                 .= _config_genesisFile
  <*> Toml.table rpcCodec       "rpc"                          .= _config_rpc
  <*> Toml.table p2pCodec       "p2p"                          .= _config_p2p
  <*> Toml.table consensusCodec "consensus"                    .= _config_consensus
  <*> Toml.table instrCodec     "instrumentation"              .= _config_instrumentation

rpcCodec :: TomlCodec ConfigRPC
rpcCodec = pure ConfigRPC
  <*> uri                       "laddr"                        .= _configRPC_laddr
  <*> Toml.arrayOf Toml._Text   "cors_allowed_origins"         .= _configRPC_allowedOrigins

p2pCodec :: TomlCodec ConfigP2P
p2pCodec = pure ConfigP2P
  <*> uri                       "laddr"                        .= _configP2P_laddr
  <*> Toml.text                 "persistent_peers"             .= _configP2P_persistentPeers
  <*> Toml.text                 "private_peer_ids"             .= _configP2P_privatePeerIds
  <*> Toml.bool                 "addr_book_strict"             .= _configP2P_addrBookStrict
  <*> Toml.bool                 "allow_duplicate_ip"           .= _configP2P_allowDuplicateIp

consensusCodec :: TomlCodec ConfigConsensus
consensusCodec = pure ConfigConsensus
  <*> Toml.text                 "create_empty_blocks_interval" .= _configConsensus_createEmptyBlocksInterval

instrCodec :: TomlCodec ConfigInstr
instrCodec = pure ConfigInstr
  <*> Toml.bool                 "prometheus"                   .= _configInstr_prometheus
  <*> Toml.text                 "prometheus_listen_addr"       .= _configInstr_prometheus_listen_addr
  <*> Toml.word                 "max_open_connections"         .= _configInstr_max_open_connections

concat <$> traverse makeLenses
  [ ''Config
  , ''ConfigRPC
  , ''ConfigP2P
  , ''ConfigConsensus
  , ''ConfigInstr
  ]
