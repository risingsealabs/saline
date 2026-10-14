module Saline.App.Editor.Utils.Reflex.Dom where

import Control.Monad.Fix
import Data.Align
import Data.Map (Map)
import Data.Text (Text)
import Data.These
import Reflex.Dom.Core hiding (tag)

import Saline.App.Editor.Utils.Reflex

elHoldAttr
  :: (DomBuilder t m, MonadFix m, MonadHold t m)
  => Text -> Hold t (Map Text Text) -> m a -> m a
elHoldAttr tag attrsH = fmap snd . elHoldAttr' tag attrsH

elHoldAttr'
  :: (DomBuilder t m, MonadFix m, MonadHold t m)
  => Text -> Hold t (Map Text Text) -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
elHoldAttr' tag h child = do
  (attrs0, attrsEv) <- unHold h
  attrsDyn <- holdDyn attrs0 attrsEv

  let
    cfg = def
      & elementConfig_initialAttributes .~ mapKeysToAttributeName attrs0
      & elementConfig_modifyAttributes .~ fmap mapKeysToAttributeName patchEv

    diff = alignWith $ \case
      This _ -> Nothing
      That b -> Just b
      These _ b -> Just b --TODO: allow deep diff/patch for b?

    patchEv = diff <$> current attrsDyn <@> attrsEv

  element tag cfg child
