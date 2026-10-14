{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonoLocalBinds #-}
module Saline.App.Editor.Diagrams where

import Control.Lens ((^.))
import Data.Bool
import Data.Functor
import Data.Functor.Identity
import Data.Maybe
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Tuple
import Language.Javascript.JSaddle (liftJSM)
import Language.Javascript.JSaddle.Object (js, js4, jsg)
import Reflex.Dom.Core
import Saline.App.Editor.PrettyPrint qualified as PrettyPrint
import Saline.Crypto
import Saline.Intents.Condition
import Saline.Intents.Diagrams

diagramSymbolic
  :: (DomBuilder t m, Prerender t m)
  => Condition Expr Expr []
  -> m ()
diagramSymbolic = diagram PrettyPrint.expr (PrettyPrint.expr addressEllipsis) id

diagramConcrete
  :: (DomBuilder t m, Prerender t m)
  => Condition Identity Identity []
  -> m ()
diagramConcrete = diagram (. runIdentity) (addressEllipsis . runIdentity) id

diagram
  :: (DomBuilder t m, Prerender t m, Ord (e Amount), Ord (p Address))
  => (forall x. (x -> Text) -> (e x -> Text))
  -> (p Address -> Text)
  -> (forall x. l x -> [x])
  -> Condition e p l
  -> m ()
diagram ppExpr ppParty expandList i = prerender_ blank $ do
  (anchor, ()) <- elAttr' "div" ("id" =: "diagram") blank

  let
    pp_assets = PrettyPrint.assets $ ppExpr PrettyPrint.amount

    collect f = Set.toList $ Set.fromList $ f =<< edges

    edges = ffor (Set.toList $ collectEdges expandList i) $ \(Edge t s g) ->
      let (source, target) = bool swap id (_transfer_hostRole t == Role_Source) ("<intent>", ppParty (_transfer_counterparty t))
      in
        ( source
        , target
        , pp_assets (_transfer_assets t)
        , s
        , isNothing g
        )
    nodes = collect $ \(t, f, _, _, _) -> [t, f]
    groups = [0,1] :: [Word]

  liftJSM $ void
    $ jsg ("window" :: Text)
    ^. js ("saline" :: Text)
    ^. js4 ("diagram" :: Text) (_element_raw anchor) nodes edges groups
