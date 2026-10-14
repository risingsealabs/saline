{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Saline.Network.CometBFT.CLI where

import Control.Concurrent.Async (Async, async)
import Control.Exception (IOException, handle)
import Control.Lens (imap, makeLenses, (%~), (&), (.~), (^.), _Just, _Right)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Bool (bool)
import Data.Functor (void)
import Data.List.NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)
import GHC.Generics (Generic)
import Shelly (Sh, cp, pwd, run, shelly, silently, toTextIgnore, withTmpDir, (</>))
import Shelly qualified as Sh
import System.Console.ANSI
import Text.URI.Lens (authHost, authPort, uriAuthority)

import Saline.Network.CometBFT.Config hiding (uri)
import Saline.Network.CometBFT.Node
import Saline.Utils
import System.Which (staticWhich)

cliEnv :: ThreadEnv
cliEnv = ThreadEnv
  { _threadEnv_component = "CLI"
  , _threadEnv_color = Cyan
  }

newtype GlobalFlags = GlobalFlags
  { _globalFlags_home :: Text
  } deriving stock (Eq, Ord, Read, Show, Generic)

data NetworkFlags = NetworkFlags
  { _networkFlags_validators    :: Word
  , _networkFlags_output        :: Text
  , _networkFlags_populatePeers :: Bool
  , _networkFlags_startingIP    :: Text
  } deriving stock (Eq, Ord, Read, Show, Generic)

mkNetworkFlags :: NodeEnvironment -> Text -> Word -> NetworkFlags
mkNetworkFlags env networkRoot size = NetworkFlags
  { _networkFlags_validators    = size
  , _networkFlags_output        = networkRoot
  , _networkFlags_populatePeers = True
  , _networkFlags_startingIP    = case env of
      NodeEnvironment_Container -> "192.167.10.2"
      NodeEnvironment_Thread -> localhost
  }

loadCometBFTNode :: MonadIO m => Text -> m CometBFTNode
loadCometBFTNode home = pure (CometBFTNode home)
  <*> loadConfigFile (home <> "/config" <> "/config.toml")
  <*> shelly (silently $ fmap Text.strip $ tendermint (GlobalFlags home) "show_node_id" [])

storeConfig :: MonadIO m => Text -> Config -> m ()
storeConfig home = storeConfigFile (home <> "/config" <> "/config.toml")

genesisFile :: CometBFTNode -> Sh.FilePath
genesisFile n = configDir n </> ("genesis.json" :: Text)

configDir :: CometBFTNode -> Sh.FilePath
configDir n = _cometBFTNode_home n </> ("config" :: Text)

tendermintPath :: Sh.FilePath
tendermintPath = Sh.fromText $ Text.pack $(staticWhich "tendermint")

tendermint :: GlobalFlags -> Text -> [Text] -> Sh Text
tendermint gf tmCmd cmdArgs = run tendermintPath $ tmArgs <> [tmCmd] <> cmdArgs
  where
    tmArgs = ["--home", _globalFlags_home gf]

tendermintNetwork :: NetworkFlags -> Sh Text
tendermintNetwork nf = run tendermintPath $ ("testnet" :) $
  [ "--v", tshow $ _networkFlags_validators nf
  , "--o", _networkFlags_output nf
  , "--starting-ip-address", _networkFlags_startingIP nf
  ] <> bool [] ["--populate-persistent-peers"] (_networkFlags_populatePeers nf)

cometBFTNode :: GlobalFlags -> Maybe (NonEmpty Text) -> Sh Text
cometBFTNode gf = tendermint gf "node" . maybe [] toList

data NodePorts = NodePorts
  { _nodePorts_p2p :: Word
  , _nodePorts_rpc :: Word
  , _nodePorts_abci :: Word
  , _nodePorts_prometheus :: Word
  } deriving stock (Eq, Ord, Read, Show, Generic)

nodePortsOffset :: Word
nodePortsOffset = 10

nthNodePorts :: Word -> NodePorts
nthNodePorts index =
  let offset = 26656 + nodePortsOffset * index
  in NodePorts offset (offset + 1) (offset + 2) (offset + 3)

addCometBFTNode :: MonadIO m => Text -> Text -> NodePorts -> CometBFTNode -> m CometBFTNode
addCometBFTNode home moniker ports preExistingNode = shelly $ do
  void $ tendermint (GlobalFlags home) "init" []
  n <- loadCometBFTNode home

  cp (genesisFile preExistingNode) (configDir n)
  let
    newCfg = _cometBFTNode_config preExistingNode
      & config_moniker .~ moniker
      & updatePorts ports
  storeConfig home newCfg

  pure $ n &
    cometBFTNode_config .~ newCfg

runNetworkNodeDir
  :: MonadIO m
  => AppNetwork q s
  -> Text
  -> m (Async (), Async ())
runNetworkNodeDir withNode dir = loadCometBFTNode dir >>= runNetworkNode withNode

runNetworkNode :: MonadIO m => AppNetwork q s -> CometBFTNode -> m (Async (), Async ())
runNetworkNode = runNode Nothing . withNetworkNode
  where withNetworkNode = _appNetwork_withNode <*> _appNetwork_queryEncoder <*> _appNetwork_genesis

runNode :: MonadIO m => Maybe (NonEmpty Text) -> (CometBFTNode -> IO ()) -> CometBFTNode -> m (Async (), Async ())
runNode passthrough withNode n = do
  initProcess

  --TODO: if the consensus process is still running, the proxy app probably should just log errors and restart
  cometAsync <- liftIO . async $ do
    flip runReaderT cliEnv $ logger $ "Launching " <> n ^. cometBFTNode_home
    void . shelly $ cometBFTNode (GlobalFlags $ _cometBFTNode_home n) passthrough
  salineAsync <- liftIO $ async (withNode n)
  return (cometAsync, salineAsync)

loadNetwork :: MonadIO m => Text -> Word -> m (Either IOException [CometBFTNode])
loadNetwork root size = liftIO $ handle onFailure $ do
  fmap Right $ for [0..size-1] $ \i -> do
    loadCometBFTNode $ root <> "/node" <> tshow i
  where
    onFailure = pure . Left @IOException

--TODO: move to dev package
initNetwork :: MonadIO m => NodeEnvironment -> Text -> Word -> Bool -> m [CometBFTNode]
initNetwork env root size withInstrumentation = shelly $ do
  void $ tendermintNetwork $ mkNetworkFlags env root size
  for [0..size-1] $ \i -> do
    let
      home = root <> "/node" <> tshow i
    n <- loadCometBFTNode home
    let
      imapWord f = imap $ \x y -> f (toEnum x) y
      oldCfg = n ^. cometBFTNode_config
      ports = nthNodePorts i

      peers = Text.splitOn "," $ oldCfg ^. config_p2p . configP2P_persistentPeers
      peers' = Text.intercalate "," $ flip imapWord peers $ \j peer ->
        let (nid, rest) = cleave "@" peer
            (_host, port) = cleave ":" rest
            port' = case treadMaybe port of
              Nothing -> error "parsing error"
              Just p -> p + j * nodePortsOffset
        in nid <> "@" <> localhost <> ":" <> tshow port'

    let cfg = oldCfg
          & config_moniker .~ "node" <> tshow i
          & config_p2p . configP2P_addrBookStrict .~ False
          & config_consensus . configConsensus_createEmptyBlocksInterval .~ "0s"
          & config_rpc . configRPC_allowedOrigins .~ ["*"]
          & case env of
              NodeEnvironment_Container -> config_rpc . configRPC_laddr . uriAuthority . _Right . authHost .~ everyInterfaceRText
              NodeEnvironment_Thread -> foldl (.) id
                [ updatePorts ports
                , config_p2p . configP2P_persistentPeers .~ peers'
                , config_p2p . configP2P_privatePeerIds .~ peers'
                , config_p2p . configP2P_allowDuplicateIp .~ True
                ]
          & case withInstrumentation of
              False -> id
              True  -> foldl (.) id
                [ config_instrumentation . configInstr_prometheus .~ True
                , config_instrumentation . configInstr_prometheus_listen_addr .~ (":" <> tshow (_nodePorts_prometheus ports))
                , config_instrumentation . configInstr_max_open_connections .~ 10
                ]

    storeConfig home cfg
    pure $ n & cometBFTNode_config .~ cfg

updatePorts :: NodePorts -> Config -> Config
updatePorts ports cfg = cfg
  & config_p2p . configP2P_laddr %~ f _nodePorts_p2p
  & config_rpc . configRPC_laddr %~ f _nodePorts_rpc
  -- TODO: update instrumentation too?
  & config_proxyApp %~ f _nodePorts_abci
  where
    f field h = h
      & uriAuthority . _Right . authPort . _Just .~ field ports
      & uriAuthority . _Right . authHost .~ localhostRText

withTempDir :: MonadIO m => (Text -> IO a) -> m a
withTempDir f = shelly . withTmpDir $ liftIO . f . toTextIgnore

-- Cannot resume network
-- https://github.com/f-o-a-m/kepler/issues/217
withCurrentDir :: MonadIO m => (Text -> IO a) -> m a
withCurrentDir f = shelly $ pwd >>= liftIO . f . toTextIgnore . (</> (".testnet" :: Text))

withNetwork :: AppNetwork q s -> ((Text -> IO a) -> IO a) -> (Text -> [CometBFTNode] -> IO a) -> IO a
withNetwork net withDir f = withDir $ \root -> do
  let
    size = _appNetwork_size net
    report outcome = flip runReaderT cliEnv $ logger (outcome <> " network of size " <> tshow size <> " at " <> root)

  loadNetwork root size >>= \case
    Left _ -> do
      genesisNodes <- initNetwork (_appNetwork_nodeEnvironment net) root size (_appNetwork_instrumentation net)
      f root genesisNodes

    Right ns -> do
      report "Restored"
      f root ns

concat <$> traverse makeLenses
  [ ''GlobalFlags
  , ''NetworkFlags
  ]
