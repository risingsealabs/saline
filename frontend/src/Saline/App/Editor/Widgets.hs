{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Saline.App.Editor.Widgets where

import Clay qualified
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Text (Text)
import Data.Traversable
import Data.Tuple
import Reflex.Dom.Core

import Saline.App.Editor.Utils.Reflex
import Saline.App.Layout

labeled :: DomBuilder t m => Text -> m a -> m a
labeled lbl a = do
  el "label" $ text lbl
  indented a

--TODO: avoid delay hack
-- `_dropdown_change` triggers with `k0` for some reason
-- but it happens after a getPostBuild (judging by traceEvent and align)
-- maybe it's on hydration switchover?
dropdownUniq
  :: (Ord k, DomBuilder t m, MonadFix m, MonadHold t m, MonadIO (Performable m), PostBuild t m, PerformEvent t m, TriggerEvent t m)
  => k -> Dynamic t (Map k Text) -> DropdownConfig t k -> m (Event t k)
dropdownUniq k0 options cfg = do
  dd <- dropdown k0 options cfg
  delay 0 $ _dropdown_change dd

dropdownUniqHold :: (DomBuilder t m, Ord k) => k -> Map k Text -> SelectElementConfig er t (DomBuilderSpace m) -> m (Hold t k)
dropdownUniqHold k0 options cfg = dropdownWithDefault options k0 cfg

dropdown'
  :: (DomBuilder t m, MonadFix m, Ord k, MonadHold t m)
   => k -> Map k Text -> SelectElementConfig er t (DomBuilderSpace m) -> m (Event t k)
dropdown' k0 options cfg = do
  (_, ev) <- unHold =<< dropdownWithDefault options k0 cfg
  pure ev

-- https://github.com/reflex-frp/reflex-dom/issues/454
dropdownWithDefault
  :: (DomBuilder t m, Ord a)
  => Map a Text
  -> a
  -> SelectElementConfig er t (DomBuilderSpace m)
  -> m (Hold t a)
dropdownWithDefault options start cfg = mdo
  let
    cfg' = cfg & selectElementConfig_initialValue .~ fromMaybe "" (Map.lookup start options)
    options' = Map.fromList $ fmap swap $ Map.toList options

  (selectEl, _) <- selectElement cfg' $ fmap leftmost $ for (snd <$> Map.toList options) $ \optText -> do
    (e, _) <- elAttr' "option" ("value" =: optText) $ text optText
    pure $ optText <$ domEvent Click e
  pure $ mkHold start $ fmapMaybe (flip Map.lookup options') $ _selectElement_change selectEl

borderCss :: Clay.Size Clay.LengthUnit -> Clay.Stroke -> Clay.Color -> Clay.Css
borderCss size stroke color = do
  Clay.borderWidth size
  Clay.borderStyle stroke
  Clay.borderColor color

moatCss :: Maybe Clay.Color -> Clay.Css
moatCss c = do
  Clay.sym Clay.margin (Clay.px 5)
  Clay.sym Clay.padding (Clay.px 5)
  for_ c $ borderCss (Clay.px 1) Clay.dotted
