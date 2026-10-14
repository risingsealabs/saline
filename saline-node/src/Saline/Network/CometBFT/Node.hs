{-# LANGUAGE TemplateHaskell #-}

module Saline.Network.CometBFT.Node where

import Control.Lens (makeLenses)
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.URI (URI)

import Saline.Interface.Query (CometQueryEncoder)
import Saline.Network.CometBFT.Config hiding (uri)

data NodeEnvironment
  = NodeEnvironment_Thread
  | NodeEnvironment_Container
  deriving stock (Eq, Ord, Read, Show, Generic)

--TODO: split into network-dev-specific and node-executable-relevant
data AppNetwork q genesis = AppNetwork
  { _appNetwork_queryEncoder :: CometQueryEncoder q
  , _appNetwork_size :: Word
  , _appNetwork_nodeEnvironment :: NodeEnvironment
  , _appNetwork_genesis :: genesis
  , _appNetwork_instrumentation :: Bool
  , _appNetwork_withNode :: CometQueryEncoder q -> genesis -> CometBFTNode -> IO ()
  }

data CometBFTNode = CometBFTNode
  { _cometBFTNode_home        :: Text
  , _cometBFTNode_config      :: Config
  , _cometBFTNode_id          :: Text
  } deriving stock (Eq, Ord, Show, Generic)

concat <$> traverse makeLenses
  [ ''AppNetwork
  , ''CometBFTNode
  ]

getNodeURI :: CometBFTNode -> URI
getNodeURI = _configRPC_laddr . _config_rpc . _cometBFTNode_config
