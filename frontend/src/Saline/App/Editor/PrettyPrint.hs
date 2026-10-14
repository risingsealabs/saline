{-# LANGUAGE OverloadedStrings #-}
module Saline.App.Editor.PrettyPrint where

import Control.Arrow ((>>>))
import Data.Bool (bool)
import Data.ByteArray.HexString (HexString (unHexString))
import Data.Dependent.Sum
import Data.Foldable
import Data.Functor.Identity
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Saline.Crypto
import Saline.Intents.Condition
import Saline.Utils hiding (timestamp)
import Saline.Utils qualified as Utils

amount :: Amount -> Text
amount = tshow

address :: PublicKey -> Text
address = addressEllipsis

word :: Word -> Text
word = tshow

token :: Token -> Text
token = tshow

var :: Var a -> Text
var (Var _ name) = "$" <> name

timestamp :: Timestamp -> Text
timestamp = Utils.timestamp . realToFrac

-- TODO: can't seem to write with pattern synonyms as inference breaks
expr :: (a -> Text) -> Expr a -> Text
expr render = unNewtypeDSum >>> \case
  ExprTag_Receive :=> Identity f -> flow "Incoming" f
  ExprTag_Send :=> Identity f -> flow "Outgoing" f
  ExprTag_Lit :=> Identity l -> render l
  ExprTag_Balance :=> Identity t -> token t <> " balance"
  ExprTag_Var :=> Identity v -> var v
  ExprTag_Arithmetic2 :=> Identity (Binary x op y) -> fold
    [ "("
    , expr render x
    , symbol op
    , expr render y
    , ")"
    ]

flow :: Text -> Token -> Text
flow direction t = direction <> token t

assets :: (f Amount -> Text) -> AssetF f Map -> Text
assets famount = Text.intercalate ", " . fmap f . Map.toAscList
  where
    f (t, a) = famount a <> " " <> token t

-- Display a number with the thousands separator, eg 12345 -> "12,345"
ppWithThousands :: (Show a) => a -> Text
ppWithThousands = Text.reverse . Text.intercalate " " . Text.chunksOf 3 . Text.reverse . tshow

ppHexString :: HexString -> Text
ppHexString = ("0x" <>) . bytestring2hex . unHexString

-- Pretty prints a dollar amount with thousands separator and 2-digits cents
ppUSDAmount :: Bool -> Amount -> Text
ppUSDAmount dollarSign amount_ =
  let
    amountWithDecimals = prettyPrintWithDecimals False 2 amount_
    (doll, cents) = Text.break (=='.') amountWithDecimals
    ppDoll = ppWithThousands ((tread doll) :: Integer)
    ppTotalAmount = (bool "" "$" dollarSign) <> ppDoll <> cents
  in ppTotalAmount
