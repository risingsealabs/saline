module Saline.App.Environment where

import Control.Monad ((<=<))
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class
import Data.Aeson qualified as Aeson
import Data.Dependent.Map hiding (ffor)
import Data.Functor
import Data.Functor.Compose
import Data.Functor.Identity (Identity(..))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Data.Dependent.Map qualified as DMap
import Data.Semigroup
import Reflex.Dom.Core
import Obelisk.Route
import Text.URI (URI)

import Saline.App.Storage (Entry, Storage)
import Saline.App.Utils
import Saline.Crypto
import Saline.Interface.Query
import Saline.Network.Node.RPC qualified as RPC
import Saline.Network.Node.RPC (RPC)

type SelectedWallet = Maybe (Arg BIP32Child SecretKey)
data Env t = Env
  { _env_nodes :: NonEmpty URI
  , _env_queryEncoder :: SalineEncoder
  , _env_autoUnlock :: Maybe Password --TODO: rewire so this code path only exists in dev and is alltogether absent from production build
  , _env_storage :: DMap Entry (Compose (Dynamic t) Maybe)
  , _env_storage' :: Storage --TODO: normalize
  , _env_selectedWallet :: Dynamic t SelectedWallet
  }

readStorage :: Reflex t => Env t -> Entry a -> Dynamic t (Maybe a)
readStorage env k = maybe (pure Nothing) getCompose $ DMap.lookup k $ _env_storage env

writeStorage :: (EventWriter t (DMap Entry Identity) m, Reflex t) => Entry a -> Event t a -> m ()
writeStorage k ev = tellEvent $ ffor ev $ DMap.singleton k . Identity

poll
  :: (Eq a, MonadFix m, PerformEvent t m, Prerender t m)
  => Env t -> NominalDiffTime -> RPC a -> m (Event t (Either Text a))
poll env period f = fmap switchDyn $ prerender (pure never) $ do
  let node = NonEmpty.head $ _env_nodes env
  ev <- tickLossyFromPostBuildTime period
  pb <- getPostBuild
  holdUniqEv <=< performEvent $ liftIO (f node) <$ leftmost [void ev, pb]

rpcDyn
  :: (Eq a, DomBuilder t m, MonadFix m, MonadHold t m, Prerender t m, PostBuild t m, PerformEvent t m)
  => Env t -> Dynamic t (RPC a) -> m (Event t (Either Text a))
rpcDyn env rpc = switchHold never <=< dyn $ ffor rpc $ poll env 1

pollDyn
  :: ( Eq b, Show b, Aeson.FromJSON b
     , DomBuilder t m, MonadHold t m, Prerender t m, PostBuild t m, PerformEvent t m, MonadFix m
     )
  => Env t -> SalineQuery (Req a b) -> Dynamic t a -> m (Event t (Either Text b))
pollDyn env k vDyn = rpcDyn env $ ffor vDyn $ \v -> RPC.query (_env_queryEncoder env) (CometQuery_Store :/) k v
