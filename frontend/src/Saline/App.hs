{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
module Saline.App where

import Clay qualified
import Control.Monad (when)
import Control.Monad.Fix
import Data.Bool
import Data.Dependent.Map (DMap)
import Data.Foldable
import Data.Functor.Identity
import Data.Some
import Data.These (These (..))
import Data.These.Combinators (justThere, justHere)
import Language.Javascript.JSaddle (MonadJSM, ToJSVal (..))
import Obelisk.Frontend
import Obelisk.Generated.Static (static)
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core

import Saline.App.Assets (logo, img)
import Saline.App.Editor.Main (editor)
import Saline.App.Environment
import Saline.App.Explorer
import Saline.App.Layout
import Saline.App.Storage qualified as Storage
import Saline.App.Tailwind
import Saline.App.Wallet
import Saline.App.Wallet.Setup
import Saline.Config
import Saline.Crypto (SecretKey, Password)
import Saline.Interface.Query
import Saline.Interface.Route

frontend :: Frontend (R AppRoute)
frontend = Frontend frontendHead (frontendBody True Nothing)

frontendHead :: ObeliskWidget t (R AppRoute) m => RoutedT t (R AppRoute) m ()
frontendHead = do
  el "title" $ text "Saline Wallet"
  -- Adding Roboto font through CDN (probably a bad idea for Prod)
  elAttr "link" ("rel" =: "preconnect" <> "href" =: "https://fonts.googleapis.com") blank
  elAttr "link" ("rel" =: "preconnect" <> "href" =: "https://fonts.gstatic.com" <> "crossorigin" =: "") blank
  elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://fonts.googleapis.com/css2?family=Roboto:ital,wght@0,100;0,300;0,400;0,500;0,700;0,900;1,100;1,300;1,400;1,500;1,700;1,900&display=swap") blank

  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1.0") blank

  let js src = elAttr "script" ("type" =: "application/javascript" <> "src" =: src) blank --TODO: vendor? sha256?

  js "https://cdn.jsdelivr.net/npm/d3@7"

  -- Adding Tailwindcss through online CDN for dev purpose (should not be used for prod: slow)
  js "https://cdn.tailwindcss.com?plugins=forms,typography"

  -- Adding apexchart for graphs, charts and such
  js "https://cdn.jsdelivr.net/npm/apexcharts@3.49.1"

  js $(static "js/bip39.js")
  js $(static "js/bls12-381.js")
  js $(static "js/bls-hd-key.js")
  js $(static "js/tendermint.js")

  js $(static "js/lib.js")

  -- Extending Tailwindcss' 'sans' to include and prefer Roboto
  el "script" $ text "tailwind.config = {theme: {fontFamily: {'sans': 'Roboto, ui-sans-serif, system-ui, sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto Color Emoji\"'}, extend: {colors: {saline: '#009FC0', 'saline-light': '#00B7DB', 'menu-bg': '#211F26', 'btn-inactive': '#1D1B20', 'input-bg': '#2B2930', 'panel-bg': '#2B2930', 'panel-block-bg': '#1A1A22'}}}}"

  el "style" $ text "::backdrop { background: white; opacity: 0.2; }"

frontendBody :: Bool -> Maybe Password -> ObeliskWidget t (R AppRoute) m => RoutedT t (R AppRoute) m ()
frontendBody showTestnetBanner autoUnlock = withNodesConfig getConfigFeedbackDOM $ \nodes -> do
  case checkEncoder salineQueryEncoder of
    Left err -> text err
    Right enc -> elAttr "div" (style rootstyle) $ prerender_ blank $ Storage.withStorage $ \s views -> mdo
      walletEv <- body showTestnetBanner $ Env nodes enc autoUnlock views s walletDyn
      walletDyn <- holdDyn Nothing walletEv
      pure ()
  where
    rootstyle = do
      Clay.background (Clay.rgb 22 22 22)
      Clay.color Clay.white
      Clay.height (Clay.vh 100)
      Clay.width (Clay.vw 100)
      Clay.overflow Clay.auto

body
  :: forall t m
   . ( DomBuilder t m, MonadFix m, MonadHold t m, MonadJSM m, MonadJSM (Performable m)
     , PerformEvent t m, PostBuild t m, Prerender t m, TriggerEvent t m
     , RouteToUrl (R AppRoute) m
     , SetRoute t (R AppRoute) m
     , EventWriter t (DMap Storage.Entry Identity) m
     , ToJSVal (RawElement (DomBuilderSpace m))
     )
  => Bool -> Env t -> RoutedT t (R AppRoute) m (Event t SelectedWallet)
body showTestnetBanner env = do

  divClass "flex flex-col h-full" $ mdo
    let bannerClass = "flex h-8 shrink-0 text-center place-items-center bg-saline text-white font-semibold"
                   <> " tracking-wide select-none cursor-default"
    when showTestnetBanner $
      divClass bannerClass $ elClass "p" "flex-1 text-center" $ text "SALINE TESTNET - ALPHA VERSION"
    (sessionEv0 :: Event t (Maybe SecretKey), ev) <- runWithReplace loggedOut $ fmap app $ leftmost [sessionEv0, sessionEv]
    sessionEv <- switchHold sessionEv0 $ fmapMaybe justThere <$> ev
    switchHold never $ ffor ev $ fmapMaybe justHere

  where
    loggedOut = unlockWith env

    unlockWith unlockEnv = setup unlockEnv >>= \case
      Left a -> fmap (Just a <$) getPostBuild
      Right a -> pure $ fmap Just a

    app :: Maybe SecretKey -> RoutedT t (R AppRoute) m (Event t (These SelectedWallet (Maybe SecretKey)))
    app = \case
      -- Explicit locking must not immediately reuse the development password.
      Nothing -> (fmap . fmap) That $ unlockWith env { _env_autoUnlock = Nothing }
      Just sk -> divClass "flex flex-row flex-1" $ do
        column navStyle $ do
          menu

        divClass "flex flex-col w-full overflow-auto" $ do
          fmap switchDyn $ subRoute $ \case
            AppRoute_Dashboard -> (fmap . fmap) This $ dashboard env sk
            AppRoute_Intents   -> fmap (fmap This . switchDyn) $ subRoute $ \case
              IntentsRoute_Editor -> (never <$) $ editor env sk
              IntentsRoute_Fulfill -> fulfillIntents env sk $ hoist IntentsRoute_Fulfill
              IntentsRoute_Manage -> manageIntents env sk
              where hoist me r = AppRoute_Intents :/ me :/ r
            AppRoute_Explorer  -> never <$ explorer env
            AppRoute_Settings  -> fmap (That Nothing <$) $ settings env

    navStyle = do
      Clay.background (Clay.rgb 15 15 15)

-- | Menu bar, always present, on the left of the screen
menu
  :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m, Prerender t m, RouteToUrl (R AppRoute) m, SetRoute t (R AppRoute) m)
  => RoutedT t (R AppRoute) m ()
menu = mdo
  -- This is an "overlay" mask that hides (and prevents click) the app while the mobile menu is opened.
  -- It is only displayed in mobile/small screens
  let menuBgMaskCommon = "absolute h-screen w-screen bg-black/20 backdrop-blur-sm z-30" -- the z-40 is only useful because the Apex chart goes over otherwise
                <> md "hidden"
      dynMenuBgMaskHidden = ffor dynShownMobileMenu $ bool "hidden" ""
      dynMenuBgMaskClass = dynMenuBgMaskHidden <> " " <> constDyn menuBgMaskCommon
  (mask, _) <- elDynClass' "div" dynMenuBgMaskClass blank
  let evMaskClick = domEvent Click mask -- capture click to be able to close menu by clicking on "empty space"

  -- The menu itself
  dr <- askRoute
  let dmx = demux $ ffor dr $ \(fragment :/ _) -> Some fragment

      menuCommonClass = "absolute bg-menu-bg h-full font-light text-sm w-48 px-4 z-40" -- the z-40 is only useful because the Apex chart goes over otherwise
               <> md "relative block"
      dynMenuHideClass = ffor dynShownMobileMenu $ bool "hidden" ""
      dynMenuClass = dynMenuHideClass <> " " <> constDyn menuCommonClass

  -- Include a close mobile menu button (only on mobile/small screen)
  evCloseMenu <- elDynClass "div" dynMenuClass $ do
    let closeBurgerClass = "absolute top-0 right-0 w-8"
                        <> md "hidden"
    evCloseMenu' <- divClass closeBurgerClass $ do
      (closeBtn, _) <- elClass' "button" "w-full" $ do
        img $(static "img/icon_close.svg") $ pure $ \attrs -> attrs
          <> "alt" =: "Close mobile menu"
      pure $ domEvent Click closeBtn

    -- TODO: make the logo changed based on the subroute
    logo $ pure "w-full"

    topMenuItem dmx AppRoute_Dashboard
      ("Dashboard", DashboardRoute_Summary   :/ SummaryRoute_Assets :/ ())
      [ ("Summary", DashboardRoute_Summary   :/ SummaryRoute_Assets :/ ())
      , ("Send",    DashboardRoute_Operation :/ OperationRoute_Send :/ ())
      , ("Trade",   DashboardRoute_Operation :/ OperationRoute_Trade :/ ())
      , ("Other",   DashboardRoute_Operation :/ OperationRoute_Other :/ Nothing)
      ]

    topMenuItem dmx AppRoute_Explorer
      ("Explore", Just (ExplorerRoute_Intents :/ ()))
      [ ("Blocks" , Just (ExplorerRoute_Block   :/ Nothing))
      , ("Intents", Just (ExplorerRoute_Intents :/ ()))
      ]

    topMenuItem dmx AppRoute_Intents
      ("Intents", IntentsRoute_Manage :/ ())
      [ ("Editor", IntentsRoute_Editor :/ Nothing)
      , ("Fulfill", IntentsRoute_Fulfill :/ Nothing)
      , ("My intents", IntentsRoute_Manage :/ ())
      ]

    topMenuItem dmx AppRoute_Settings
      ("Settings", ())
      [
      ]

    pure evCloseMenu'

  -- Burger button to display mobile menu (only shown in mobile/small screen)
  let burguerClass = "bg-menu-bg h-full w-10 px-1 pt-1" <> md "hidden"
  dynShownMobileMenu <- toggle False $ leftmost [evBurgerClick, evCloseMenu, evMaskClick]

  evBurgerClick <- divClass burguerClass $ do
    (burgerBtn, _) <- elClass' "button" "w-full" $ do
      -- Display burger menu icon or close icon based on whether the mobile menu is already shown
      img $(static "img/icon_burger.svg") $ pure $ \attrs -> attrs
        <> "alt" =: "Mobile menu"
    pure $ domEvent Click burgerBtn

  pure ()

  where
    -- Top menu link, always shown
    topMenuItem dmx root (name, branch0) branches = do
      let isActive = demuxed dmx (Some root)
          activeAttrs = bool "text-white" "text-saline font-bold bg-saline-light/20"
          linkAttrs = ffor isActive $ \active -> activeAttrs active <> " " <> ("group flex items-center gap-2 my-1 p-2 h-10 cursor-pointer rounded" <> hover "font-normal text-saline bg-saline-light/20")
      routeLinkDynAttr (fmap ("class" =:) linkAttrs) (pure $ root :/ branch0) $ do
        -- let hexagon = bool "hexagon.svg" "hexagon-blue.svg"
        let wHexagon = $(static "img/hexagon.svg")
            bHexagon = $(static "img/hexagon-blue.svg")
            hexagon = bool wHexagon bHexagon
            classes = ffor isActive $ \active ->
              "flex-none h-3/4 w-4 bg-no-repeat bg-[url('" <> hexagon active <> "')]" <> grpHover ("bg-[url('" <> bHexagon <>"')]")
        elDynAttr "div" (fmap ("class" =:) classes) blank
        text name
      subRoute_ $ \fragment -> when (Some fragment == Some root) $ for_ branches $ \(n,path) -> routeLink (root :/ path) $ subMenuItem n
    -- Submenu from a top menu, only shown when current top-menu is active
    subMenuItem name = divClass ("flex items-center ml-8 p-2 text-neutral-300 font-extralight cursor-pointer h-9" <> hover "font-light text-neutral-100 bg-zinc-600/50") $ text name
