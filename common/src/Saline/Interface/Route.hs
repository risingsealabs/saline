{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Saline.Interface.Route where

import Control.Category
import Control.Lens (strict)
import Control.Monad (join)
import Control.Monad.Except (MonadError (throwError))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Functor.Identity
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Text
import Data.Text qualified as Text
import Obelisk.Route
import Obelisk.Route.TH
import Prelude hiding (id, (.))

import Saline.Crypto (BIP32Child, PublicKey, publicKeyEncoder, hexEncoder)
import Saline.Intents
import Saline.Intents.Condition
import Saline.Network.Node.Transaction

type SignableTx = Either Tx SignedTx

-- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.
data AppRoute :: Type -> Type where
  AppRoute_Intents   :: AppRoute (R IntentsRoute)
  AppRoute_Explorer  :: AppRoute (Maybe (R ExplorerRoute))
  AppRoute_Dashboard :: AppRoute (R DashboardRoute)
  AppRoute_Settings  :: AppRoute ()

data ExplorerRoute :: Type -> Type where
  ExplorerRoute_Address :: ExplorerRoute PublicKey
  ExplorerRoute_Block   :: ExplorerRoute (Maybe Height)
  ExplorerRoute_Intents :: ExplorerRoute ()
  ExplorerRoute_Tx      :: ExplorerRoute (Hash SignedTx)

data DashboardRoute :: Type -> Type where
  DashboardRoute_Operation :: DashboardRoute (R OperationRoute)
  DashboardRoute_Summary :: DashboardRoute (R SummaryRoute)

data SummaryRoute :: Type -> Type where
  SummaryRoute_Assets  :: SummaryRoute ()
  SummaryRoute_Wallets :: SummaryRoute ()

data OperationRoute :: Type -> Type where
  OperationRoute_Send    :: OperationRoute ()
  OperationRoute_Trade   :: OperationRoute ()
  OperationRoute_Other   :: OperationRoute (Maybe SignableTx)

data ContractsRoute :: Type -> Type where
  ContractsRoute_Explore :: ContractsRoute ()
  ContractsRoute_ForSale :: ContractsRoute ()
  ContractsRoute_Duties  :: ContractsRoute ()
  ContractsRoute_Rights  :: ContractsRoute (Maybe (PublicKey, Intent))

data IntentsRoute :: Type -> Type where
  IntentsRoute_Editor  :: IntentsRoute (Maybe (R EditorRoute))
  IntentsRoute_Fulfill :: IntentsRoute (Maybe PublicKey)
  IntentsRoute_Manage  :: IntentsRoute ()

data EditorRoute :: Type -> Type where
  EditorRoute_Custom  :: EditorRoute (VariableNames, Intent)
  EditorRoute_Library :: EditorRoute (R LibraryRoute)

data LibraryRoute :: Type -> Type where
  LibraryRoute_Delegate   :: LibraryRoute ()
  LibraryRoute_Dusting    :: LibraryRoute ()
  LibraryRoute_Faucet     :: LibraryRoute ()
  LibraryRoute_Forward    :: LibraryRoute ()
  LibraryRoute_Limited    :: LibraryRoute ()
  LibraryRoute_Multisig   :: LibraryRoute ()
  LibraryRoute_Swap       :: LibraryRoute ()
  LibraryRoute_SwapHybrid :: LibraryRoute ()

fmap join $ traverse deriveRouteComponent
  [ ''AppRoute
  , ''ContractsRoute
  , ''DashboardRoute
  , ''OperationRoute
  , ''EditorRoute
  , ''ExplorerRoute
  , ''IntentsRoute
  , ''LibraryRoute
  , ''SummaryRoute
  ]

type FrontendOnlyRoute = FullRoute Void1 -- Makes backend routes impossible
type AppEncoder = Encoder Identity Identity (R (FrontendOnlyRoute AppRoute)) PageName

fullRouteEncoder :: Encoder (Either Text) Identity (R (FrontendOnlyRoute AppRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder missing (\case) $ \case
  AppRoute_Intents   -> PathSegment "intents" $ pathComponentEncoder $ \case
    IntentsRoute_Editor -> PathSegment "editor" editorRouteEncoder
    IntentsRoute_Fulfill -> PathSegment "fulfill" $ singlePathSegmentEncoder . maybeEncoder (unitEncoder mempty) publicKeyEncoder
    IntentsRoute_Manage -> PathSegment "manage" $ unitEncoder mempty
  AppRoute_Explorer  -> PathSegment "explorer" $ maybeEncoder (unitEncoder mempty) $ pathComponentEncoder $ \case
    ExplorerRoute_Address  -> PathSegment "address" $ singlePathSegmentEncoder . publicKeyEncoder
    ExplorerRoute_Block    -> PathSegment "block" $ maybeEncoder (unitEncoder mempty) unsafeShowEncoder
    ExplorerRoute_Intents  -> PathSegment "intents" $ unitEncoder mempty
    ExplorerRoute_Tx       -> PathSegment "tx" $ singlePathSegmentEncoder . hashEncoder
  AppRoute_Dashboard -> PathSegment "dashboard" $ pathComponentEncoder $ \case
    DashboardRoute_Summary -> PathSegment "summary" $ pathComponentEncoder $ \case
      SummaryRoute_Assets  -> PathSegment "assets" $ unitEncoder mempty
      SummaryRoute_Wallets -> PathSegment "wallets" $ unitEncoder mempty
    DashboardRoute_Operation -> PathSegment "details" operationRouteEncoder
  AppRoute_Settings -> PathSegment "settings" $ unitEncoder mempty
  where
    missing = FullRoute_Frontend (ObeliskRoute_App AppRoute_Dashboard) :/ DashboardRoute_Summary :/ SummaryRoute_Assets :/ ()

editorRouteEncoder :: Encoder (Either Text) (Either Text) (Maybe (R EditorRoute)) PageName
editorRouteEncoder = maybeEncoder (unitEncoder mempty) $ pathComponentEncoder $ \case
  EditorRoute_Custom  -> PathSegment "custom" $ singlePathSegmentEncoder . jsonEncoder
  EditorRoute_Library -> PathSegment "library" $ pathComponentEncoder $ \case
    LibraryRoute_Delegate   -> PathSegment "delegate" $ unitEncoder mempty
    LibraryRoute_Dusting    -> PathSegment "dusting" $ unitEncoder mempty
    LibraryRoute_Faucet     -> PathSegment "faucet" $ unitEncoder mempty
    LibraryRoute_Forward    -> PathSegment "forward" $ unitEncoder mempty
    LibraryRoute_Limited    -> PathSegment "limited" $ unitEncoder mempty
    LibraryRoute_Multisig   -> PathSegment "multisig" $ unitEncoder mempty
    LibraryRoute_Swap       -> PathSegment "swap" $ unitEncoder mempty
    LibraryRoute_SwapHybrid -> PathSegment "hybridSwap" $ unitEncoder mempty

walletEncoder
  :: (Applicative check, MonadError Text parse, check ~ parse)
  => Encoder check parse (Maybe BIP32Child) Text
walletEncoder = maybeEncoder (unitEncoder mempty) unsafeTshowEncoder

operationRouteEncoder :: (MonadError Text parse, check ~ parse) => Encoder check parse (R OperationRoute) PageName
operationRouteEncoder =  pathComponentEncoder $ \case
  OperationRoute_Send  -> PathSegment "send" $ unitEncoder mempty
  OperationRoute_Trade -> PathSegment "trade" $ unitEncoder mempty
  OperationRoute_Other -> PathSegment "other" $ maybeEncoder (unitEncoder mempty) $ singlePathSegmentEncoder . signableTxEncoder

-- TODO: add tests checking roundtrip
hashEncoder :: Applicative check => Encoder check (Either Text) (Hash SignedTx) Text
hashEncoder = unsafeMkEncoder $ EncoderImpl parseHashTx renderHash

-- TODO: use binary encoding directly instead of going through json
signableTxEncoder
  :: (MonadError Text parse, Monad check)
  => Encoder check parse (Either Tx SignedTx) Text
signableTxEncoder = unsafeSumEncoder overlapCheck txEncoder signedTxEncoder
  where
    -- TODO: we rely on the fact that the json encodings (used by `signedTxEncoder`) can not overlap
    -- When we switch to a more efficient binary encoding for this we can probably
    -- supply a proof here that they have non-overlapping prefixes
    overlapCheck _ _ = pure ()

instructionsEncoder :: (Applicative check, MonadError Text parse) => Encoder check parse (NonEmpty Instruction) Text
instructionsEncoder = jsonHexEncoder

txEncoder :: (Applicative check, MonadError Text parse) => Encoder check parse Tx Text
txEncoder = jsonHexEncoder

signedTxEncoder :: (Applicative check, MonadError Text parse) => Encoder check parse SignedTx Text
signedTxEncoder = jsonHexEncoder

-- TODO: use binary encoding directly instead of going through json
jsonHexEncoder :: (Applicative check, MonadError Text parse, Aeson.FromJSON a, Aeson.ToJSON a) => Encoder check parse a Text
jsonHexEncoder = hexEncoder True . viewEncoder strict . jsonEncoderByteString

jsonEncoderByteString
  :: (Aeson.FromJSON a, MonadError Text parse, Aeson.ToJSON a, Applicative check)
  => Encoder check parse a BSL.ByteString
jsonEncoderByteString = unsafeMkEncoder $ EncoderImpl (either (throwError . Text.pack) pure . Aeson.eitherDecode) Aeson.encode
