{-# LANGUAGE OverloadedStrings #-}

module Saline.App.Tailwind where

import Data.Text (Text)
import Data.Text qualified as Text

tailwindPrefixWith :: Text -> Text -> Text
tailwindPrefixWith p = (" " <>) . Text.unwords . fmap (\classes -> p <> ":" <> classes) . Text.words

-- Styles applied conditionally
disabled, hover, focus, grpHover, dark, md, sm, lg, xl, xxl, placeholder, invalid, valid, oor :: Text -> Text
disabled    = tailwindPrefixWith "disabled"
hover       = tailwindPrefixWith "hover"
focus       = tailwindPrefixWith "focus"
grpHover    = tailwindPrefixWith "grpHover"
dark        = tailwindPrefixWith "dark"
md          = tailwindPrefixWith "md"
sm          = tailwindPrefixWith "sm"
lg          = tailwindPrefixWith "lg"
xl          = tailwindPrefixWith "xl"
xxl         = tailwindPrefixWith "2xl"
placeholder = tailwindPrefixWith "placeholder"
invalid     = tailwindPrefixWith "invalid"
valid       = tailwindPrefixWith "valid"
oor         = tailwindPrefixWith "out-of-range"
