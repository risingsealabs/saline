{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Saline.App.Wallet where

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative(liftA2)
#endif
import Control.Error (hush, note)
import Control.Lens (ix, set, (^.))
import Control.Monad (join, (<=<))
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Aeson (FromJSON (parseJSON))
import Data.Bifunctor
import Data.Bool (bool)
import Data.Dependent.Map (DMap)
import Data.Dependent.Sum ((==>))
import Data.Foldable
import Data.Functor (void)
import Data.Functor.Compose
import Data.Functor.Identity
import Data.List.NonEmpty (nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Universe (Finite(universeF))
import Language.Javascript.JSaddle (MonadJSM, ToJSVal (..), js, js1, js2, jsg, liftJSM)
import Obelisk.Generated.Static (static)
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Witherable

import Saline.App.Editor.Utils.Reflex
import Saline.App.Editor.PrettyPrint (ppUSDAmount)
import Saline.App.Environment
import Saline.App.Storage qualified as Storage
import Saline.App.Tailwind
import Saline.App.Utils
    ( addClass, setPlaceholder, tokenColor, textAreaField )
import Saline.App.Widgets
import Saline.Crypto (BIP32Child, KeyTree (..), PublicKey, SecretKey, addressEllipsis, addressEllipsisBig, deriveLeaf, derivePublicKey, mkTree, prettyPrintAddress, publicKeyEncoder)
import Saline.Intents (Intent)
import Saline.Intents.Condition
import Saline.Interface.Route
import Saline.Network.Node.Transaction
import Saline.Network.Node.RPC (callbackFromJS)
import Saline.Interface.Query
import Saline.Utils
import Saline.Utils.Uniques qualified as Uniques

type Alias = Text

pollAllBalances
  :: (DomBuilder t m, MonadHold t m, Prerender t m, PostBuild t m, PerformEvent t m, MonadFix m)
  => Env t -> SecretKey -> m (Event t (Either Text (Map Token Amount)))
pollAllBalances env sk = do
  let
    -- Derive the public keys from the wallet's indexes
    leafKey :: BIP32Child -> PublicKey
    leafKey idx = derivePublicKey $ _keyTree_privateKey $ deriveLeaf idx $ mkTree sk

  -- Get the list of wallets
  let dynWallets = maybe [] Uniques.getUniques <$> readStorage env Storage.Entry_Wallets
  pollDyn env SalineQuery_Aggregate $ ffor dynWallets $ Set.fromList . fmap leafKey . zipWith const [0..]

-- | Dashboard page : displays summary of wallets, assets in wallets, allows to add or remove wallets
dashboard
  :: forall t m
   . ( DomBuilder t m, MonadFix m, MonadHold t m, MonadJSM m, MonadJSM (Performable m)
     , PerformEvent t m, PostBuild t m, Prerender t m, TriggerEvent t m
     , RouteToUrl (R AppRoute) m
     , SetRoute t (R AppRoute) m
     , EventWriter t (DMap Storage.Entry Identity) m
     , ToJSVal (RawElement (DomBuilderSpace m))
     )
  => Env t -> SecretKey -> RoutedT t (R DashboardRoute) m (Event t SelectedWallet)
dashboard env sk = fmap switchDyn $ subRoute $ \case
  DashboardRoute_Summary -> never <$ summaryScreen env sk
  DashboardRoute_Operation -> fmap switchDyn $ subRoute $ \case
    OperationRoute_Send -> withBalances $ \walletBalanceDyn dsk -> send env dsk walletBalanceDyn
    OperationRoute_Trade -> withBalances $ \walletBalanceDyn dsk -> trade env dsk walletBalanceDyn
    OperationRoute_Other -> sign env sk $ hoist . (OperationRoute_Other :/)
  where
    hoist r = AppRoute_Dashboard :/ DashboardRoute_Operation :/ r

    withBalances
      :: forall r. (Dynamic t (Maybe (Map Token Amount))
      -> Dynamic t SecretKey -> RoutedT t r m ())
      -> RoutedT t r m (Event t SelectedWallet)
    withBalances = withChildWallet env sk (assetsTable True noAddrErrMsgs. fmap balancesSummary) . leftPanelWithBalances env

    noAddrErrMsgs = ["No assets in address.", "Please provision or select another."]

-- | Display the $-equivalent amount of all aggregated wallets, and the
-- hexagone tiled shortcut navigation buttons
balanceSummaryAndButtons
  :: (DomBuilder t m, SetRoute t (R AppRoute) m, PostBuild t m, RouteToUrl (R AppRoute) m, Prerender t m)
  => Dynamic t BalancesSummary -> m ()
balanceSummaryAndButtons dynSummary = do
  -- Display the $-equivalent balance of the selected wallet (or all wallets if chosen)
  let titleClass = "font-bold text-saline text-4xl text-center my-3"
                <> md "text-5xl"
  elClass "h2" titleClass $ dynText $ ppUSDAmount True . snd <$> dynSummary

  -- Display buttons to send, receive,  trade, etc.
  let btnContainerClass = "flex place-content-center my-3 gap-5 text-neutral-500 text-center font-light text-sm"
  divClass btnContainerClass $ do
    operationItem "Send"    $(static "img/icon_send.svg")    $ OperationRoute_Send :/ ()
    operationItem "Trade"   $(static "img/icon_trade.svg")   $ OperationRoute_Trade :/ ()
    operationItem "Other"   $(static "img/icon_other.svg")   $ OperationRoute_Other :/ Nothing

  where
    -- One of the icon button to trigger send, receive, trade action
    operationItem name icon r = do
      routeLinkAttr ("class" =: (hover "font-normal")) (AppRoute_Dashboard :/ DashboardRoute_Operation :/ r) $ do
        elAttr "img" ("src" =: icon <> "class" =: ("border border-saline rounded-full" <> md "border-0 rounded-none")) blank
        text name

--TODO: come up with some subSetRoute util instead of manually threading around route hoisting functions
withChildWallet
  :: (DomBuilder t m, MonadFix m, MonadHold t m, MonadIO (Performable m), PerformEvent t m, PostBuild t m, TriggerEvent t m)
  => Env t
  -> SecretKey
  -> (Dynamic t a -> RoutedT t r m ())
  -> (RoutedT t r m (Dynamic t (Maybe a)))
  -> RoutedT t r m (Event t SelectedWallet)
withChildWallet env sk rightPanel leftPanel = do
  let panelsOuterClass = "flex flex-col flex-1" -- layout is "top/down" panel on mobile
                      <> md "flex-row" -- layout is "left/right" panel on larger screens
  divClass panelsOuterClass $ do
    -- Left panel: show the dropdown and the left-panel widget passed in param
    let leftPanelClass = "flex flex-col w-full bg-panel-bg p-3"
                      <> md "w-96" -- on non-mobile, panel is left, so needs a fixed size
    (userInput, output) <- divClass leftPanelClass $ do

      let
        walletDyn = (fmap . fmap) (derivePublicKey . argValue) $ _env_selectedWallet env

      ev <- unHoldDyn $ (fmap . fmap) argKey $ _env_selectedWallet env
      userInput <- switchHold never . snd <=< runWithReplace blank $ ffor ev $ uncurry $ aliasPicker env sk

      let
        addrDisp dynPubkey' = divClass "px-3 text-sm" $ do
          elClass "pre" "text-saline" $ dynText $ addressEllipsis <$> dynPubkey'

      -- Display the selected wallet's address (for UX hint and avoid mistake)
      dynMaybe_ blank addrDisp walletDyn

      fmap (userInput,) leftPanel

    let
      rightPanelClass = "flex flex-col flex-1 justify-center items-center p-2"
                     <> md "p-16"
      noSelection = divClass "flex flex-col justify-center h-full text-3xl" $ text "Please choose a wallet"

    -- Right panel
    divClass rightPanelClass $ do
      dynMaybe_ noSelection rightPanel output

    pure userInput

leftPanelWithBalances
  :: (DomBuilder t m, MonadFix m, MonadHold t m, PerformEvent t m, Prerender t m, PostBuild t m)
  => Env t
  -> (Dynamic t (Maybe (Map Token Amount)) -> Dynamic t SecretKey -> m ())
  -> m (Dynamic t (Maybe (Map Token Amount)))
leftPanelWithBalances env f = do
  let dynWallet = _env_selectedWallet env
  respEv <- switchHold never <=< dyn $ ffor dynWallet $ \case
    Nothing -> pure never
    Just w -> pollDyn env SalineQuery_Aggregate $ pure $ Set.singleton $ derivePublicKey $ argValue w

  dynBalances <- holdDyn Nothing $ leftmost
    [ Nothing <$ updated dynWallet
    , hush <$> respEv
    ]
  -- Keep left panel blank until user selected a wallet
  -- TODO replace blank by a nicer "wait for input" message widget
  let
    mkLeftPanel walletDyn balancesDyn = do
      f balancesDyn (fmap argValue walletDyn)

  dynMaybe_ blank (uncurry mkLeftPanel . splitDynPure) $ ffor2 dynWallet dynBalances $ \mw bs -> ffor mw (,bs)

  pure dynBalances

fulfillIntents
  :: ( DomBuilder t m, MonadFix m, MonadHold t m, MonadJSM m, MonadJSM (Performable m), PerformEvent t m, Prerender t m, PostBuild t m, TriggerEvent t m
     , SetRoute t (R AppRoute) m
     , RouteToUrl (R AppRoute) m
     , ToJSVal (RawElement (DomBuilderSpace m))
     )
  => Env t
  -> SecretKey
  -> (Maybe PublicKey -> R AppRoute)
  -> RoutedT t (Maybe PublicKey) m (Event t SelectedWallet)
fulfillIntents env masterKey hoist = do
  withChildWallet env masterKey (dyn_ . fmap rightPanel) $
    holdDyn Nothing . fmap Just <=< switchHold never <=< leftPanel hoist $ _env_selectedWallet env
  where
    leftPanel subHoist = dynMaybe (pure never) $ \selection -> divClass "flex flex-col items-center gap-8" $ do
      let dsk = fmap argValue selection
      rDyn <- askRoute
      ev <- unHoldDyn rDyn
      switchHold never . snd <=< runWithReplace blank $ ffor ev $ \(tx0, txEv) -> do
        let
          val0 = val tx0
          val = maybe (Left "") Right
          txt = either id (encode @(Either Text) publicKeyEncoder) . val

        userInput <- fmap updated $ addressSearch "HOST" $ \cfg -> cfg
          & setPlaceholder "Host"
          & set inputElementConfig_initialValue (txt tx0)
          & set inputElementConfig_setValue (fmap txt txEv)

        hostDyn <- (fmap . fmap) (first $ const "Invalid address") $ holdDyn val0 $ leftmost
          [ userInput
          , fmap val txEv
          ]

        setRoute $ fmap (subHoist . Just) $ snd $ fanEither userInput

        intentDyn <- renderIntent hostDyn

        pure $ updated $ pure (,,) <*> dsk <*> hostDyn <*> intentDyn

    renderIntent hostDyn = do
      intentEv <- switchHold never =<< dynEither (\e -> never <$ dynText e) (embedIntent env) hostDyn
      holdDyn Nothing $ fmap (join . hush) intentEv

    rightPanel (sk, ehost, i) = case ehost of
      Left err -> text err
      Right host -> solver env sk host i id

manageIntents
  :: ( DomBuilder t m, MonadFix m, MonadHold t m, MonadJSM m, MonadJSM (Performable m), PerformEvent t m, Prerender t m, PostBuild t m, TriggerEvent t m
     , SetRoute t (R AppRoute) m
     , RouteToUrl (R AppRoute) m
     , ToJSVal (RawElement (DomBuilderSpace m))
     )
  => Env t
  -> SecretKey
  -> RoutedT t () m (Event t SelectedWallet)
manageIntents env masterKey = withChildWallet env masterKey rightPanel leftPanel
  where
    rightPanel = void . embedIntent env

    leftPanel = divClass "flex flex-col items-center" $ do
      txButtons env "Delete intent"   id $ mkTx $ simpleTx . pure . Instruction_Delete
      txButtons env "Disable address" id $ mkTx $ \owner -> simpleTx $ pure $ Instruction_SetIntent owner $ Condition_Any 1 []

      pure $ (fmap . fmap) (derivePublicKey . argValue) $ _env_selectedWallet env

    mkTx f = fmap (mk . fmap argValue) $ _env_selectedWallet env
      where
        mk = \case
          Nothing -> Left "No wallet selected"
          Just sw -> Right $ second Right $ prepareTx sw f

embedIntent
  :: (DomBuilder t m, MonadFix m, MonadHold t m, PerformEvent t m, PostBuild t m, Prerender t m)
  => Env t -> Dynamic t PublicKey -> m (Event t (Either Text (Maybe Intent)))
embedIntent env dpk = do
  resp <- pollDyn env SalineQuery_Intent dpk
  -- TODO: allow poll to tag the response instead
  dyn_ $ ffor dpk $ \pk -> do
    void $ runWithReplace blank $ ffor resp $ either text $ intent . maybe (Left pk) Right
  pure resp

-- | Summary screen of the dashboard, shows wallet and assets categories
summaryScreen
  :: ( DomBuilder t m, MonadHold t m, MonadFix m, MonadJSM m, MonadJSM (Performable m)
     , PerformEvent t m, PostBuild t m, Prerender t m, TriggerEvent t m
     , RouteToUrl (R AppRoute) m
     , SetRoute t (R AppRoute) m
     , EventWriter t (DMap Storage.Entry Identity) m
     , ToJSVal (RawElement (DomBuilderSpace m))
     )
  => Env t -> SecretKey -> RoutedT t (R SummaryRoute) m ()
summaryScreen env sk = do
  tokenBalances <- eitherDyn =<< holdDyn (Right mempty) =<< pollAllBalances env sk
  dyn_ $ ffor tokenBalances $ \case
    Left err -> dynText ("ERROR while querying the wallets: " <> err)
    Right eiBalances -> do
      let summary = ffor eiBalances balancesSummary

      balanceSummaryAndButtons summary
      let summaryClass = "flex flex-col" -- vertical screens on mobile
                      <> md "flex-row" -- horizontal layout on largers screens
      divClass summaryClass $ do
        -- Graph representation of tokens diversification
        let outerGraphClass = "w-full h-48 text-neutral-300"
                           <> md "w-5/12 h-96"
        divClass outerGraphClass $ do
          divClass "flex place-content-center pt-2" $ do
            chartContainer <- toJSVal . _element_raw . fst <$> elClass' "div" "pt-2" blank

            (initEv :: Event t (), initCb) <- newTriggerEvent

            callbacks <- liftJSM
              $ jsg ("window" :: Text)
              ^. js ("saline" :: Text)
              ^. js2 ("initTokenChart" :: Text) chartContainer (callbackFromJS parseJSON (liftIO . initCb))

            let
              tokenData (t, b) = (tshow t, _balance_inUSD b, tokenColor t)
              updateChart bs = void $ liftJSM $ callbacks ^. js1 ("updateChart" :: Text) (fmap tokenData $ Map.toList bs)

            loaded <- holdDyn False $ True <$ initEv
            performEvent_ $ fmap (updateChart . fst) $ leftmost
              [ current summary <@ initEv
              , gate (current loaded) $ updated summary
              ]

        -- Table representation of tokens diversification
        divClass "flex-1 p-2" $ do
          -- Buttons to select between assets and wallets
          divClass ("flex gap-3 text-neutral-500 my-3 text-xs font-semibold uppercase") $ do
            routeToggleGroup (\r -> AppRoute_Dashboard :/ DashboardRoute_Summary :/ r)
              [ ("Assets", SummaryRoute_Assets ==> ())
              , ("Wallets", SummaryRoute_Wallets ==> ())
              ]

          subRoute_ $ \case
            SummaryRoute_Assets -> assetsTable False noWalletErrMsg summary
            SummaryRoute_Wallets -> walletsList env sk -- just type-checking
  where
    noWalletErrMsg :: [Text]
    noWalletErrMsg = ["No assets at any address.", "Please provision some funds."]

-- | In the Summary screen, display the different owned wallets, with their names, value, tokens, etc.
walletsList :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m, EventWriter t (DMap Storage.Entry Identity) m) => Env t -> SecretKey -> m ()
walletsList env sk = do
  let tableClass = "w-full font-light divide-y divide-neutral-600 text-sm"
                <> md "text-base"
  evRename <- elAttr "table" ("class" =: tableClass) $ do
    -- Set columns width (re-used, hence why in variable)
    let nameW = "w-4/12" <> md "w-3/12"
        addrW = "w-5/12" <> md "w-6/12"
        copyW = "w-1/12"
        pathW = "w-2/12 hidden" <> md "inline" -- we hide the derivation path on mobile: too small, not _that_ important

    -- Table headers
    elClass "tr" ("text-neutral-300 text-left text-xs ") $ forM_
      [("Wallet Name", nameW)
     , ("Saline Address", addrW)
     , ("Copy", copyW)
     , ("Derivation Path", pathW)
     ] $ \(title, w) -> do
      elClass "th" ("font-light text-left" <> " " <> w) $ text title

    -- Display wallets name and address
    walletRenames <- listViewWithKey (Map.fromList <$> dynIdxWallets) $ \idx dynWalletName -> do
      elClass "tr" ("h-9 text-sm w-full" <> hover "font-medium") $ do
        let root = mkTree sk
            node = deriveLeaf (toEnum idx) root

        -- Display wallet name
        evRename <- elClass "td" ("text-left max-w-0 pr-2" <> " " <> nameW) $ mdo
          -- Style for the input containning wallet names
          let inputClassAttrs = "w-full rounded-sm bg-input-bg text-sm p-1" <> focus "ring-1 ring-saline"
              nameChangePendingClass = "text-orange-500"
              newInput = _inputElement_value iE
              dynAreEqual = ffor2 newInput dynWalletName (==)
              -- Change the <input> style based on whether the user has unsaved rename
              attrs = ffor (updated dynAreEqual) $ bool
                (  "class" =: Just (inputClassAttrs <> " " <> nameChangePendingClass)
                <> "title" =: Just "Press Enter to confirm rename (unsaved change)"
                )
                (  "class" =: Just inputClassAttrs
                <> "title" =: Just "Edit wallet name and press Enter to rename"
                )

          evPostBuild <- getPostBuild

          iE <- inputElement $ def
            & initialAttributes .~ ( "type" =: "text"
                                  <> "class" =: inputClassAttrs
                                  <> "title" =: "Press Enter to rename wallet" )
            & modifyAttributes .~ attrs
            & inputElementConfig_setValue .~ (current dynWalletName <@ evPostBuild)
          let pressEvt = domEvent Keypress iE
              code = keyCodeLookup . fromIntegral <$> pressEvt
          pure $ tag (current $ _inputElement_value iE) (Witherable.filter (== Enter) code)

        -- Display wallet Saline address (nacl:xxx)
        -- let naclAddress = prettyPrintAddress $ derivePublicKey $ _keyTree_privateKey node
        let naclAddress = derivePublicKey $ _keyTree_privateKey node
        elClass "td" ("text-left text-xs font-mono max-w-0 overflow-hidden text-ellipsis pr-2" <> " " <> addrW) $ do
          -- Showing the standard (small) address ellipsis on big screen make it look awfully empty. So what we do is we display
          -- both a normal (short) ellipsised address and one with more numbers and we use CSS to hide or of the other based on the
          -- screen size. This allows to have decent visual of the address based on sceen size
          elClass "span" (md "hidden") $ text (addressEllipsis naclAddress)
          elClass "span" ("hidden" <> md "inline") $ text (addressEllipsisBig naclAddress)

        -- Display copy icon
        elClass "td" ("text-left h-8" <> " " <> copyW) $ do
          let copyFunction = "copyToClipboard('" <> prettyPrintAddress naclAddress <> "', 'Saline address copied to clipboard')"
          elAttr "button" ("onclick" =: copyFunction <> "title" =: "Copy Saline address") $ do
            elAttr "img" ("src" =: $(static "img/copy-icon.svg") <> "class" =: "h-5 cursor-copy") blank

        -- Display Key Path (BIP44 HD Key deriv)
        -- NOTE: this is hidden om mobile
        elClass "td" ("text-left text-xs max-w-0" <> " " <> pathW) $ do
          text $ _keyTree_keyPath node

        pure evRename

    -- Form to add/create new wallet
    evNewWallets <- elClass "tr" ("h-9 text-sm w-full" <> hover "font-medium") $ do
      evNewWalletName <- elClass "td" ("text-left max-w-0 pr-2" <> " " <> nameW) $ mdo
        let inputClassAttrs = "w-full rounded-sm bg-input-bg text-sm p-1" <> focus "ring-1 ring-saline"
        -- Text input to enter new wallet name
        iE <- inputElement $ def
            & initialAttributes .~ ( "type" =: "text"
                                  <> "class" =: inputClassAttrs
                                  <> "placeholder" =: "New wallet name"
                                  <> "title" =: "Enter new wallet name and press Enter"
                                    )
            & inputElementConfig_setValue .~ ("" <$ evEnterPressed)
        -- Hint text to specify that user needs to press enter to comfirm. Only show
        -- when text have been entered in the input
        let hintCommonClasses = "text-xs italic text-center text-slate-400"
            dynHintAttrs = ffor dynNewWalletName $ bool
              ("class" =: (hintCommonClasses <> " " <> "visible"))
              ("class" =: (hintCommonClasses <> " " <> "invisible"))
              . Text.null
        elDynAttr "div" dynHintAttrs $ do
          text "(Press enter to confirm)"

        let dynNewWalletName = _inputElement_value iE
            pressEvt = domEvent Keypress iE
            code = keyCodeLookup . fromIntegral <$> pressEvt
            evEnterPressed = (Witherable.filter (== Enter) code)

        pure $ fforMaybe (current (ffor2 dynWallets dynNewWalletName (,)) <@ evEnterPressed) $ \(currentWallets, newWalletName) -> do
          if Text.null newWalletName then
            Nothing
          else
            Uniques.list $ currentWallets <> [newWalletName]

      evDeleteLastWallet <- elClass "td" ("text-left text-xs font-mono max-w-0 overflow-hidden text-ellipsis pr-2 relative bottom-2" <> " " <> addrW) $ do
        evDelClk <- deleteButton (not . null <$> dynWallets) id $ do
          trashIcon
          elClass "span" ("flex h-full items-center pr-1") $ text "Delete last wallet"
        pure $ fforMaybe (current dynWallets <@ evDelClk) $ \ws -> do
          ws' <- nonEmpty ws
          Uniques.list $ NonEmpty.init ws'

      pure $ leftmost [evNewWalletName, evDeleteLastWallet]

    pure $ leftmost [ fforMaybe (attach (current dynWallets) walletRenames) $ \(currentNames, renames) -> do
      Uniques.list $ foldl overwrite currentNames $ Map.toList renames
                   , evNewWallets]


  writeStorage Storage.Entry_Wallets evRename

  where
    -- Read wallets names from storage
    dynWallets = maybe [] Uniques.getUniques <$> readStorage env Storage.Entry_Wallets
    dynIdxWallets = zip [0..] <$> dynWallets -- Pair those wallets with index

    overwrite xs (i,name) = xs & ix i .~ name

label :: DomBuilder t m => Text -> m ()
label = el "label" . text

labeled :: DomBuilder t m => Text -> m a -> m a
labeled lbl a = divClass "flex flex-row items-center gap-4" $ label lbl *> a

send
  :: ( DomBuilder t m, MonadFix m, MonadHold t m, MonadJSM m, MonadJSM (Performable m),PerformEvent t m, Prerender t m, PostBuild t m
     , SetRoute t (R AppRoute) m
     , RouteToUrl (R AppRoute) m
     , ToJSVal (RawElement (DomBuilderSpace m))
     )
  => Env t -> Dynamic t SecretKey -> Dynamic t (Maybe (Map Token Amount)) -> m ()
send env dsk dynAssets = do
  let dynIsWalletNonEmpty = not . Map.null . fromMaybe mempty <$> dynAssets

  dyn_ $ ffor dynIsWalletNonEmpty $ bool emptyPanel $ do
    -- Block to choose currency and amount
    transfer <- block "grid grid-cols-2" $ \lbl -> do
      lbl "CURRENCY"
      lbl "AVAILABLE"
      lbl ""
      evMaxClicked <- maxButton
      dynToken <- tokenDropdown True dynAssets
      dynAmount <- amountInput dynAssets evMaxClicked dynToken
      pure $ liftA2 (,) (Compose dynToken) (Compose dynAmount)

    -- Block to enter destination wallet address
    to <- addressSearch "TO WALLET" id

    -- Send button
    txButtons env "Send" id $ getCompose $ pure tx <*> Compose (fmap pure dsk) <*> Compose to <*> transfer


  where
    tx sk to (token, amount) = second Right $ prepareTx sk $ \from ->
      disregardingInstalled from $ simpleTx $ pure $ Instruction_Funds from to $ token =: amount

    -- Button used to fill the field with the maximum value
    maxButton = do
      (e, _) <- elClass' "button" "text-saline text-sm text-left" $ text "Max"
      pure $ domEvent Click e

trade
  :: ( DomBuilder t m, MonadFix m, MonadHold t m, MonadJSM m, MonadJSM (Performable m), PerformEvent t m, Prerender t m, PostBuild t m
     , SetRoute t (R AppRoute) m
     , RouteToUrl (R AppRoute) m
     , ToJSVal (RawElement (DomBuilderSpace m))
     )
   => Env t -> Dynamic t SecretKey -> Dynamic t (Maybe (Map Token Amount)) -> RoutedT t () m ()
trade env dsk dynAssets = do
  let dynIsWalletNonEmpty = not . Map.null . fromMaybe mempty <$> dynAssets
  dyn_ $ ffor dynIsWalletNonEmpty $ bool emptyPanel $ do
    swapsDyn <- limitOrderForm
    swapsTable swapsDyn

  where
    limitOrderForm = do
      (sellBuy, amounts) <- block "grid grid-cols-2 gap-4" $ \lbl ->  do
        lbl "SELL"
        lbl "BUY"

        dynSellToken <- tokenDropdown True dynAssets
        -- For the "buy" side, we want to allow buying all assets, so list them all
        -- (we don't care about balance for the buy side, so set to 0)
        let allTokens = constDyn $ Just $ Map.fromList $ map (,0) (universeF)
        dynBuyToken <- tokenDropdown False allTokens

        evMaxSellClicked <- maxButton
        el "div" blank -- just to occupy the grid spot

        dynSellAmount <- amountInput dynAssets evMaxSellClicked dynSellToken
        dynBuyAmount <- amountInput (constDyn Nothing) never dynBuyToken

        let sellBuy = (liftA2 . liftA2) (,) dynSellToken dynBuyToken
            amounts = (liftA2 . liftA2) (,) dynSellAmount dynBuyAmount

        pure (sellBuy, amounts)

      let flows = ffor2 sellBuy amounts $ liftA2 $ \(ft, bt) (fa, ba) -> ((ft, fa), (bt, ba))

      txButtons env "Place order" id $ ffor2 dsk flows $ \sk -> fmap $ \(fw, bw) -> second Right $ prepareTx sk $ \pk ->
        let
          mkExpr = uncurry Map.singleton . second Expr_Lit
        in
          simpleTx $ pure $ Instruction_OrIntent pk $ Condition_Finite 1 $ swapPolicy
            Nothing
            (mkExpr bw)
            (mkExpr fw)

      let querySwaps tokens = pollDyn env SalineQuery_Swaps $ ffor2 dsk tokens $ \sk ts -> (derivePublicKey sk, ts)

      holdDyn Nothing . fmap Just
        =<< switchHold never
        =<< dynEither ((never <$) . dynText) querySwaps sellBuy

    swapsTable = dynMaybe_ blank $ dynEither_ display $ \swapsDyn -> do
      let cls isEmpty = Map.singleton "class" $ "flex flex-col w-full " <> tableDivide <> bool "" " hidden" isEmpty
      elDynAttr "div" (ffor swapsDyn $ cls . null) $ do
        r $ do
          smallColumn $ text "Rate"
          smallColumn $ text "Sell"
          smallColumn $ text "Buy"
          largeColumn blank
        void $ listViewWithKey swapsDyn $ \t txsDyn -> do
          r $ do
            smallColumn $ text $ prettyPrintWithDecimals False 3 $ unitPrice t
            smallColumn $ text $ tshow $ fst $ unTrade t
            smallColumn $ text $ tshow $ snd $ unTrade t

            dyn_ $ ffor txsDyn $ \swaps -> for_ swaps $ \tx -> do
              let add w = Map.alter $ Just . maybe w (<> " " <> w)
              largeColumn $ txButtons env "Swap" (add "min-w-10" "class") $ ffor dsk $ \sk -> Right $ (, Right sk) $
                disregardingInstalled (derivePublicKey sk) tx

          pure never

    -- Button used to fill the field with the maximum value
    maxButton = do
      (e, _) <- elClass' "button" "text-saline text-sm text-left" $ text "Max"
      pure $ domEvent Click e

    r = divClass "flex flex-row items-center py-2"
    smallColumn = divClass "w-1/5"
    largeColumn = divClass "w-3/5"

-- TODO: do-we-want/can-we-check whether the signer is relevant for the signed transaction?
sign
  :: ( DomBuilder t m, MonadFix m, MonadHold t m, MonadJSM m, MonadJSM (Performable m), PerformEvent t m, Prerender t m, PostBuild t m, TriggerEvent t m
     , SetRoute t (R AppRoute) m
     , RouteToUrl (R AppRoute) m
     , ToJSVal (RawElement (DomBuilderSpace m))
     )
  => Env t
  -> SecretKey
  -> (Maybe SignableTx -> R AppRoute)
  -> RoutedT t (Maybe SignableTx) m (Event t SelectedWallet)
sign env masterKey hoist = do
  ev <- unHoldDyn =<< askRoute
  switchHold never . snd <=< runWithReplace blank $ ffor ev $
    withChildWallet env masterKey rightPanel . leftPanel
  where
    mkErr err = dynText $ ffor err $ \case
      "" -> ""
      _ -> "Input a valid transaction"

    rightPanel = dynEither_ mkErr $ dyn_ . fmap renderTxStandalone

    leftPanel (tx0, txEv) = divClass "flex flex-col" $ do
      let
        val0 = val tx0
        val = maybe (Left "") Right
        txt = either id (encode @(Either Text) signableTxEncoder) . val

      userInput <- textAreaField val0 (tryDecode signableTxEncoder . Text.strip) $ \cfg -> cfg
        & addClass "h-60 w-full my-2 bg-panel-block-bg"
        & addClass formFieldClass
        & set textAreaElementConfig_initialValue (txt tx0)
        & set textAreaElementConfig_setValue (fmap txt txEv)

      txDyn <- holdDyn val0 $ leftmost
        [ updated userInput
        , fmap val txEv
        ]

      signableTxButtons env "Submit" id $ ffor2 txDyn (_env_selectedWallet env) $ \etx mw -> do
        tx <- etx
        w <- note "" mw
        pure (tx, Right $ argValue w)

      let (_, parseEv) = fanEither $ updated userInput
      setRoute $ ffor parseEv $ hoist . Just
      pure $ fmap Just userInput

settings :: (DomBuilder t m, MonadJSM (Performable m), PerformEvent t m, PostBuild t m) => Env t -> m (Event t ())
settings env = divClass "flex flex-col justify-center items-center gap-16 h-full" $ do
  logout <- formButton "Lock wallet" (pure True) id
  nuke <- deleteButton (pure True) (<> " " <> "p-3") $ text "Delete all storage"
  wiped <- performEvent $ Storage.nuke (_env_storage' env) <$ nuke
  pure $ leftmost [logout, wiped]
