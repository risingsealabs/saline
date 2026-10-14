{-# LANGUAGE OverloadedStrings #-}
module Saline.App.Layout where

import Clay hiding (column, style)
import Clay qualified
import Clay.Render (htmlInline)
import Data.Map (Map)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy (toStrict)
import Reflex.Dom.Core

styleTag :: DomBuilder t m => Css -> m ()
styleTag = el "style" . text . toStrict . render

style :: (IsString s, Ord s) => Css -> Map s Text
style css = "style" =: inlineCss css

inlineCss :: Css -> Text
inlineCss = toStrict . renderWith htmlInline []

line :: DomBuilder t m => FlexDirection -> Css -> m a -> m a
line axis extra = elAttr "div" $ style $ do
  Clay.display flex
  flexDirection axis
  extra

column, row :: DomBuilder t m => Css -> m a -> m a
column = line Clay.column
row = line Clay.row

indented :: DomBuilder t m => m a -> m a
indented = elAttr "div" $ style $ marginLeft $ px 10

nested :: DomBuilder t m => m a -> m a
nested = divClass "flex flex-col items-start ml-2 pt-1"

-- https://css-shape.com/hexagon/
tile :: (DomBuilder t m, Background bg) => Size s -> bg -> Css -> m a -> m a
tile radius bg extra = elAttr "div" $ "style" =: Text.intercalate "; "
  [ "aspect-ratio: cos(30deg)"
  , "clip-path: polygon(-50% 50%,50% 100%,150% 50%,50% 0)"
  , inlineCss $ do
      height radius
      width radius
      background bg
      extra
  ]

--   TODO: this is what the mac color picker gives when used on the logo
-- but using the color picker on a circle with this color gives a darker rgb???
salineBlue :: Color
salineBlue = Clay.rgb 0x46 0x9D 0xBC
