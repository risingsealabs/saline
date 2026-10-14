{-# LANGUAGE UndecidableInstances #-}
module Saline.Intents.Diagrams where

import Data.Map (Map)
import Data.Set (Set)

import Saline.Intents.Condition

--TODO: need nonce?
--TODO: group inside each _any
data Edge assets party = Edge
  { _edge_transfer :: Transfer assets party
  , _edge_step :: Word
  , _edge_group :: Maybe Word --TODO: retain more info? e.g. cursor into intent to be able to cross-highlight on over
  }

deriving stock instance TransferConstraints a p Eq  => Eq  (Edge a p)
deriving stock instance TransferConstraints a p Ord => Ord (Edge a p)

collectEdges :: (Ord (e Amount), Ord (p Address)) => (forall x. l x -> [x]) -> Condition e p l -> Set (Edge (AssetF e Map) p)
collectEdges _ _ = mempty --TODO: use solver instead
