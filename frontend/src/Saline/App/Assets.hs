{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Saline.App.Assets where

import Data.Foldable
import Data.Map (Map)
import Data.Text (Text)
import Reflex.Dom.Core

import Obelisk.Generated.Static

import Saline.Utils

img :: (DomBuilder t m, PostBuild t m) => Dynamic t Text -> Dynamic t (Override (Map Text Text)) -> m ()
img path mkAttrs = elDynAttr "img" (ffor2 path mkAttrs $ \p f -> f $ "src" =: p) blank

imgClass :: (DomBuilder t m, PostBuild t m) => Dynamic t Text -> Map Text Text -> Dynamic t Text -> m ()
imgClass path extraBaseAttrs cls = img path $ ffor cls $ \c attrs -> fold [extraBaseAttrs, "class" =: c, attrs]

alert, logo, unlock :: (DomBuilder t m, PostBuild t m) => Dynamic t Text -> m ()
alert = imgClass $(static "img/alert.svg") mempty
unlock = imgClass $(static "img/unlock.svg") mempty
logo = imgClass $(static "img/Saline LOGO_D17.png") $ "alt" =: "Saline Logo"
