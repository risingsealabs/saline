{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Saline.App.Editor.Main where

import Clay qualified
#if !MIN_VERSION_base(4,18,0)
import Control.Applicative(liftA2)
#endif
import Control.Monad
import Control.Monad.Fix
import Data.Bifunctor
import Data.Dependent.Sum
import Data.Functor.Identity
import Data.Map qualified as Map
import Data.Some
import Language.Javascript.JSaddle
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core hiding (Alt, list, tag)

import Saline.App.Editor.Intents.Editor
import Saline.App.Editor.Render (conditionSymbolic)
import Saline.App.Editor.Scenario ()--(scenario)
import Saline.App.Editor.Utils.Reflex
import Saline.App.Environment
import Saline.App.Layout
import Saline.App.Tailwind (md)
import Saline.App.Widgets
import Saline.Crypto
import Saline.Intents
import Saline.Intents.Condition
import Saline.Intents.Instrument qualified as Instrument
import Saline.Interface.Route
import Saline.Network.Node.Transaction
import Saline.Utils
import Saline.Utils.Field

{-
contracts :: (DomBuilder t m, MonadFix m, MonadHold t m) => Hold t (Maybe (Instrument Expr)) -> m ()
contracts iHold = do
  row (Clay.justifyContent Clay.spaceAround) $ runHold_ $ ffor iHold $ traverse_ $ \i -> do
    let target = el "pre" . text . Issuances.generateSupport (Condition_All $ List_Lit [ _instrument_preCondition i, _instrument_postCondition i])
    target Contracts.Target_Ethereum
    target Contracts.Target_Solana
-}
editor
  :: forall t m
  . ( DomBuilder t m, MonadFix m, MonadHold t m, MonadJSM m, MonadJSM (Performable m)
    , PerformEvent t m, PostBuild t m, Prerender t m, TriggerEvent t m
    , SetRoute t (R AppRoute) m
    , RouteToUrl (R AppRoute) m
    , ToJSVal (RawElement (DomBuilderSpace m))
    )
  => Env t
  -> SecretKey
  -> RoutedT t (Maybe (R EditorRoute)) m ()
editor env masterKey = do
  dynRoute_ $ \mr -> do
    let
      ki :: Maybe (R EditorRoute) -> (Maybe (Some LibraryRoute), Maybe (VariableNames, Intent))
      ki = \case
        Nothing -> (Nothing, Nothing)
        Just er -> case er of
          EditorRoute_Custom  :=> Identity i -> (Nothing, Just i)
          EditorRoute_Library :=> Identity l -> (Just $ someKey l, ) $ Just $ case l of
            LibraryRoute_Delegate   :=> Identity () -> Instrument.delegate
            LibraryRoute_Dusting    :=> Identity () -> Instrument.dusting
            LibraryRoute_Faucet     :=> Identity () -> Instrument.faucet
            LibraryRoute_Forward    :=> Identity () -> Instrument.forward
            LibraryRoute_Limited    :=> Identity () -> Instrument.limited
            LibraryRoute_Multisig   :=> Identity () -> Instrument.multisig
            LibraryRoute_Swap       :=> Identity () -> Instrument.swap
            LibraryRoute_SwapHybrid :=> Identity () -> Instrument.swapHybrid

      none = (Nothing, "-- Template --")
      opts = Map.fromList $ (none:) $ fmap (first Just)
          [ (Some LibraryRoute_Delegate   , "Delegate"    )
          , (Some LibraryRoute_Dusting    , "Dusting"     )
          , (Some LibraryRoute_Faucet     , "Faucet"      )
          , (Some LibraryRoute_Forward    , "Forward"     )
          , (Some LibraryRoute_SwapHybrid , "Hybrid swap" )
          , (Some LibraryRoute_Limited    , "Limited"     )
          , (Some LibraryRoute_Multisig   , "Multisig"    )
          , (Some LibraryRoute_Swap       , "Swap"        )
          ]

      templatePicker = row (Clay.justifyContent Clay.center) $ flip withHoldM holdDyn =<< formDropdown (fst $ ki mr) opts id

    divClass ("flex flex-row w-full justify-between py-16" <> md "px-16") $ do
      iEv <- divClass "flex flex-col" $ do
        let
          mkRoute (Some l) = Just $ EditorRoute_Library :/ l :/ case l of
            LibraryRoute_Delegate   -> ()
            LibraryRoute_Dusting    -> ()
            LibraryRoute_Faucet     -> ()
            LibraryRoute_Forward    -> ()
            LibraryRoute_Limited    -> ()
            LibraryRoute_Multisig   -> ()
            LibraryRoute_Swap       -> ()
            LibraryRoute_SwapHybrid -> ()

        dlr <- templatePicker
        ev <- dyn $ ffor dlr $ \lr -> do
          a <- instrument . join . fmap (snd . ki .  mkRoute) $ lr
          unHold a

        let (ev0, evEv) = (fmap fst ev, fmap snd ev)
        ev' <- switchHold never evEv
        pure $ leftmost [ev0, ev']

      divClass "flex flex-col items-center" $ do
        let
          mkTx = \case
            Nothing -> Left ""
            Just (sw, (_, i)) -> Right $ second Right $ prepareTx (argValue sw) $ \pk ->
              simpleTx $ pure $ Instruction_SetIntent pk i

        --TODO: get rid of holdUniqDyn by making clock tick affect less things
        iDyn <- holdUniqDyn <=< holdDyn (snd $ ki mr) $ fmap justFieldValue iEv

        ev <- unHoldDyn $ (fmap . fmap) argKey $ _env_selectedWallet env

        selection <- divClass "bg-panel-bg rounded-lg" $ do
          pb <- getPostBuild
          inputEv <- switchHold never . snd <=< runWithReplace blank $ ffor ev $
            uncurry $ aliasPicker env masterKey
          holdDyn Nothing $ leftmost [inputEv, current (_env_selectedWallet env) <@ pb]

        txButtons env "Install" id $ fmap mkTx $ (liftA2 . liftA2) (,) selection iDyn

        dyn_ $ ffor iDyn $ maybe blank $ \i -> do
          divClass "flex flex-row justify-evenly w-full" $ conditionSymbolic $ snd i
          --scenario i

    pure ()
