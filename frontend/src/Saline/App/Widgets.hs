{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
module Saline.App.Widgets where

import Clay qualified
import Control.Error (hush, note)
import Control.Lens ((^.), ifor_, imap)
import Control.Monad (unless, (<=<))
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Bool (bool)
import Data.ByteArray.Base64String (Base64String)
import Data.Dependent.Sum (DSum((:=>)))
import Data.Either (isRight)
import Data.Foldable
import Data.Functor (void)
import Data.Functor.Identity
import Data.GADT.Compare
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Semigroup (Arg (..))
import Data.Set qualified as Set
import Data.Some
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import Language.Javascript.JSaddle (MonadJSM, ToJSVal (..), js, js0, js1, jsg, liftJSM)
import Obelisk.Generated.Static
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core hiding (checkbox)
import Witherable

import Saline.App.Assets
import Saline.App.Editor.PrettyPrint (ppUSDAmount)
import Saline.App.Editor.Render (conditionInstalled, hexWrapper)
import Saline.App.Editor.Utils.Reflex
import Saline.App.Editor.Widgets
import Saline.App.Environment
import Saline.App.Hardcoded
import Saline.App.Layout
import Saline.App.Storage qualified as Storage
import Saline.App.Tailwind
import Saline.App.Utils
import Saline.Crypto
import Saline.Intents (Contracts, Intent)
import Saline.Intents.Condition (Amount, Token(..))
import Saline.Interface.Route
import Saline.Network.Node.RPC qualified as RPC
import Saline.Network.Node.Transaction
import Saline.Interface.Query
import Saline.Utils
import Saline.Utils.Uniques

addressDetails
  :: (DomBuilder t m, MonadFix m, MonadHold t m, PerformEvent t m, PostBuild t m, Prerender t m)
  => Env t -> Dynamic t PublicKey ->  m ()
addressDetails env addr = divClass "flex flex-col items-center w-1/2 gap-8" $ do
  divClass "text-saline" $ hexWrapper $ dynText $ fmap addressFull addr
  resp <- pollDyn env SalineQuery_Wallet addr
  render <=< holdDyn Nothing $ fmap Just resp
  where
    render = dynLoading_ $ dynEither_ (const blank) $ \d -> do
      let (bs, dmi) = splitDynPure d
      assetsTable False noAssetsErrMsgs $ fmap balancesSummary bs
      dyn_ $ ffor2 addr dmi $ \a ->
        intent . maybe (Left a) Right

    noAssetsErrMsgs = ["No assets in address.", "Please provision or select another."]

-- TODO: is publickey/address recoverable from signature?
sig :: DomBuilder t m => Signature -> m ()
sig = hexWrapper . text . prettyPrintSignature

signedTx' :: DomBuilder t m => SignedTx -> m ()
signedTx' = hexWrapper . text . encode @(Either Text) signedTxEncoder

instructions' :: DomBuilder t m => NonEmpty Instruction -> m ()
instructions' = hexWrapper . text . encode @(Either Text) instructionsEncoder

addressLink :: (DomBuilder t m, PostBuild t m, Prerender t m, RouteToUrl (R AppRoute) m, SetRoute t (R AppRoute) m) => Dynamic t PublicKey -> m ()
addressLink dpk = dynRouteLink r $ divClass "text-saline" $ dynText $ fmap addressEllipsis dpk
  where r = ffor dpk $ \pk -> AppRoute_Explorer ?/ ExplorerRoute_Address :/ pk

intent :: (DomBuilder t m, PostBuild t m) => Either PublicKey Intent -> m ()
intent = divClass "flex flex-col items-center" . \case
  Left pk -> text "No intent installed - using default" *> conditionInstalled (defaultIntent pk)
  Right i -> conditionInstalled i

intentsGrid
  :: (Eq a, DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m)
  => Event t (Either Text (Contracts a))
  -> ((m () -> m ()) -> Intent -> Dynamic t a -> m ())
  -> m ()
intentsGrid resp mkCards = do
  let intentsClass = "grid grid-cols-1 items-center gap-x-6 gap-y-6 p-1 text-sm"
                  <> md "p-0 text-base"
                  <> lg "grid-cols-2"
                  <> xl "grid-cols-3"
  divClass intentsClass $ do
    md_ <- holdDyn Nothing $ fmap Just resp
    flip dynLoading_ md_ $ \de -> do
      ded <- eitherDyn de
      dyn_ $ ffor ded $ either display $ \dcs -> do
        void $ listWithKey dcs $ \i din -> do
          let mkCard = card $ \attrs -> pure $ attrs <> ("style" =: ("order: " <> tshow (intentSizeHeuristic i))) --TODO: is tailwind class safe here?
          mkCards mkCard i din

txButtons
  :: ( DomBuilder t m, MonadFix m, MonadHold t m, MonadJSM m, MonadJSM (Performable m), PerformEvent t m, Prerender t m, PostBuild t m
     , SetRoute t (R AppRoute) m
     , RouteToUrl (R AppRoute) m
     , ToJSVal (RawElement (DomBuilderSpace m))
     )
  => Env t
  -> Dynamic t Text
  -> Override (Map Text Text)
  -> Dynamic t (Either Text (Tx, Either Text SecretKey))
  -> m ()
txButtons env lbl override = signableTxButtons env lbl override . (fmap . fmap) (first Left)

signableTxButtons
  :: forall t m.
     ( DomBuilder t m, MonadFix m, MonadHold t m, MonadJSM m, MonadJSM (Performable m), PerformEvent t m, Prerender t m, PostBuild t m
     , SetRoute t (R AppRoute) m
     , RouteToUrl (R AppRoute) m
     , ToJSVal (RawElement (DomBuilderSpace m))
     )
  => Env t
  -> Dynamic t Text
  -> Override (Map Text Text)
  -> Dynamic t (Either Text (SignableTx, Either Text SecretKey))
  -> m ()
signableTxButtons env lbl override input = do
  let
    signedTxDyn :: Dynamic t (Either Text (Performable m SignedTx))
    signedTxDyn = ffor input $ \i -> do
      (s, esk) <- i
      sk <- esk
      pure $ case s of
        Left tx -> mkSignedTx $ prepareTx sk $ const tx
        Right stx -> pure $ addSignature sk stx

  pb <- getPostBuild
  let (signErr, signOk) = fanEither $ leftmost [ updated signedTxDyn, current signedTxDyn <@ pb]
  signedEv <- performEvent $ ffor signOk $ fmap $ encode @(Either Text) signedTxEncoder
  signedText <- holdDyn Nothing $ leftmost [ Nothing <$ signErr, fmap Just signedEv]

  let
    form enabled = divClass "flex flex-row justify-center gap-1 p-4" $ submit <* copy
      where
        submit = formButton lbl enabled override
        copy = copyToClipboardButton "Transaction" signedText

  broadcast env form signedTxDyn

broadcast
  :: ( DomBuilder t m, MonadFix m, MonadHold t m, MonadJSM m, MonadJSM (Performable m), PerformEvent t m, PostBuild t m, Prerender t m
     , SetRoute t (R AppRoute) m
     , RouteToUrl (R AppRoute) m
     , ToJSVal (RawElement (DomBuilderSpace m))
     )
  => Env t
  -> (Dynamic t Bool -> m (Event t ()))
  -> Dynamic t (Either Text (Performable m SignedTx))
  -> m ()
broadcast env mkBtn mkTxDyn = do
  clk <- mkBtn $ isRight <$> mkTxDyn
  let
    txEv = snd $ fanEither $ current mkTxDyn <@ clk
    node = NonEmpty.head $ _env_nodes env

  res <- performEvent $ ffor txEv $ liftIO . flip RPC.broadcastTxSync node <=< id

  void $ dialog $ ffor res $ \case
    Left e -> el "pre" $ never <$ text e
    Right hash -> divClass "flex flex-col items-center text-center gap-4" $ do
      el "div" $ text "Transaction submitted"
      el "div" $ text "You can monitor its status on the explorer"
      divClass "text-saline" $ transactionHash hash
      pure never

transactionHash
  :: (DomBuilder t m, Prerender t m, RouteToUrl (R AppRoute) m, SetRoute t (R AppRoute) m)
  => Hash SignedTx -> m ()
transactionHash hash = do
  let r = AppRoute_Explorer ?/ ExplorerRoute_Tx :/ hash
  routeLink r $ hexWrapper $ text $ renderHash hash

loadingWithReplace :: DomBuilder t m => Event t (m a) -> m (Event t a)
loadingWithReplace = fmap snd . runWithReplace loading

dynLoading_ :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m) => (Dynamic t a -> m ()) -> (Dynamic t (Maybe a) -> m ())
dynLoading_ = dynMaybe_ loading

dynMaybe
  :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m)
  => m b
  -> (Dynamic t a -> m b)
  -> (Dynamic t (Maybe a) -> m (Event t b))
dynMaybe n j = dyn . fmap (maybe n j) <=< maybeDyn

dynMaybe_
  :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m)
  => m ()
  -> (Dynamic t a -> m ())
  -> (Dynamic t (Maybe a) -> m ())
dynMaybe_ n j = void . dynMaybe n j

dynEither
  :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m)
  => (Dynamic t a -> m c)
  -> (Dynamic t b -> m c)
  -> (Dynamic t (Either a b) -> m (Event t c))
dynEither f g = dyn . fmap (either f g) <=< eitherDyn

dynEither_
  :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m)
  => (Dynamic t a -> m ())
  -> (Dynamic t b -> m ())
  -> (Dynamic t (Either a b) -> m ())
dynEither_ f g = void . dynEither f g

loading :: DomBuilder t m => m ()
loading = divClass (Text.unwords
  [ "h-5 w-5 rounded-full"
  , "bg-[conic-gradient(var(--tw-gradient-stops))] from-transparent to-saline"
  , "animate-spin"
  ]) blank

errorMessage :: DomBuilder t m => m a -> m a
errorMessage = elAttr "div" (style $ Clay.color Clay.red)

formDropdown
  :: (DomBuilder t m, Ord k)
  => k
  -> Map k Text
  -> Override (SelectElementConfig EventResult t (DomBuilderSpace m))
  -> m (Hold t k)
formDropdown k0 options f = dropdownUniqHold k0 options $ f $ addClass formFieldClass def

formField
  :: (DomBuilder t m, MonadHold t m)
  => a
  -> (Text -> a)
  -> Override (InputElementConfig EventResult t (DomBuilderSpace m))
  -> m (Dynamic t a)
formField val0 parse = fmap snd . formField' val0 parse

formField'
  :: (DomBuilder t m, MonadHold t m)
  => a
  -> (Text -> a)
  -> Override (InputElementConfig EventResult t (DomBuilderSpace m))
  -> m (InputElement EventResult (DomBuilderSpace m) t, Dynamic t a)
formField' val0 parse f = field' val0 parse $ f . addClass formFieldClass

formFieldClass :: IsString s => s
formFieldClass = "border-none bg-input-bg rounded-lg shadow-none"

formButton :: (DomBuilder t m, PostBuild t m) => Dynamic t Text -> Dynamic t Bool -> Override (Map Text Text) -> m (Event t ())
formButton lbl enabled override = do
  primaryButton lbl $ \attrs -> ffor enabled $ override . \case
    False -> attrs <> "disabled" =: ""
    True -> attrs

primaryButton :: (DomBuilder t m, PostBuild t m) => Dynamic t Text -> (Map Text Text -> Dynamic t (Map Text Text)) -> m (Event t ())
primaryButton txt overrideAttrs = do
  let
    attrs = overrideAttrs $ ("class" =:) $ Text.unwords
      [ "min-w-40 px-6 py-2 rounded-lg text-xs font-semibold"
      , md "text-sm px-8"
      , primaryButtonDecoration
      ]
  (e, _) <- elDynAttr' "button" attrs $ dynText txt
  pure $ domEvent Click e

primaryButtonDecoration :: Text
primaryButtonDecoration = Text.unwords
  [ "rounded-lg bg-saline"
  , hover "bg-saline-light"
  , disabled "opacity-50"
  ]

formSubmit
  :: (DomBuilder t m, PostBuild t m)
  => Dynamic t Text
  -> Dynamic t (Maybe a)
  -> m (Event t a)
formSubmit label val = mdo
  clk <- formButton label (fmap isJust val) id
  pure $ catMaybes $ tag (current val) clk

formSubmitWithCheckbox :: (DomBuilder t m, PostBuild t m, MonadHold t m) => a -> m () -> m (Event t a)
formSubmitWithCheckbox next lbl = do
  checkEv <- elAttr "label" ("class" =: "flex flex-row items-center gap-5 cursor-pointer") $ do
    ev <- checkbox False $ \cfg -> cfg
      & addClass "bg-transparent text-saline shadow-none"
      & setAttribute "style" "box-shadow: none" --TODO: figure out why this is needed for focus state even with "shadow-none" class
    lbl
    pure ev
  enabled <- holdDyn False checkEv
  ev <- formButton "Next" enabled id
  pure $ next <$ ev

--TODO: can we rely on this yet or do we need the old execCommand("copy") API?
copyToClipboard :: (MonadJSM (Performable m), PerformEvent t m) => Event t Text -> m ()
copyToClipboard ev = performEvent_ $ ffor ev $ \t ->
  void $ liftJSM $ jsg ("navigator" :: Text) ^. js ("clipboard" :: Text) . js1 ("writeText" :: Text) t

copyToClipboardButton
  :: (DomBuilder t m, MonadJSM (Performable m), PerformEvent t m, PostBuild t m)
  => Text -> Dynamic t (Maybe Text) -> m ()
copyToClipboardButton lbl txt = do
  (copySigBtn, _) <- b i
  copyToClipboard $ catMaybes $ current txt <@ domEvent Mousedown copySigBtn
  where
    b = elDynAttr' "button" $ ffor txt $ \t -> fold
      [ bool ("disabled" =: "") mempty $ isJust t
      , "class" =: (primaryButtonDecoration <> " w-8 p-2")
      ]
    i = img $(static "img/copy-icon.svg") $ pure $ \attrs -> fold
      [ attrs
      , "alt"   =: Text.unwords ["Copy", lbl]
      , "title" =: Text.unwords ["Copy", lbl, "to clipboard"]
      , "class" =: "h-5"
      ]

routeToggleButton
  :: ( GCompare r, DomBuilder t m, PostBuild t m, Prerender t m
     , RouteToUrl (R root) m
     , SetRoute t (R root) m
     )
  => Text -> (R r -> R root) -> r a -> a -> RoutedT t (R r) m ()
routeToggleButton name hoist this children = do
  dr <- askRoute
  let
    dmxRoute = demux $ ffor dr $ \(fragment :/ _) -> Some fragment
    activeClasses = bool "bg-btn-inactive text-neutral-500" "bg-saline-light text-neutral-100"
    classes = ffor (demuxed dmxRoute $ Some this) $ \active ->
      "rounded-full p-1.5 " <> activeClasses active <> " " <> hover "text-neutral-100"
  routeLinkDynAttr (fmap ("class" =:) classes) (pure $ hoist $ this :/ children) $ text name

routeToggleGroup
  :: ( GCompare r, DomBuilder t m, PostBuild t m, Prerender t m
     , RouteToUrl (R root) m
     , SetRoute t (R root) m
     )
  => (R r -> R root) -> [(Text, R r)] -> RoutedT t (R r) m ()
routeToggleGroup hoist = traverse_ $ \(lbl, this :=> Identity child) ->
  routeToggleButton lbl hoist this child

aliasPicker
  :: (DomBuilder t m, MonadFix m, MonadHold t m, MonadIO (Performable m), PerformEvent t m, PostBuild t m, TriggerEvent t m)
  => Env t
  -> SecretKey
  -> Maybe BIP32Child
  -> Event t (Maybe BIP32Child)
  -> m (Event t (Maybe (Arg BIP32Child SecretKey)))
aliasPicker env masterKey val0 valEv = divClass "w-44 bg-transparent" $ do
  let
    mkEntry i = Arg i $ leafKey i
    leafKey idx = _keyTree_privateKey $ deriveLeaf idx $ mkTree masterKey
    aliases = readStorage env Storage.Entry_Wallets
    enumerate = maybe [] $ imap (,) . getUniques
    opts = ffor aliases $ \as -> ffor (enumerate as) $ \(i, a) -> (mkEntry $ toEnum i, a)

    addNone = Map.insert Nothing "Choose a wallet" . Map.mapKeysMonotonic Just
    dropdownClass = Text.unwords
      [ "bg-transparent border-none tracking-tight font-semibold"
      , focus "border-0 ring-0"
      ]
  dropdownUniq (mkEntry <$> val0) (addNone . Map.fromList <$> opts) $ def
    & dropdownConfig_attributes .~ pure ("class" =: dropdownClass)
    & dropdownConfig_setValue .~ (fmap . fmap) mkEntry valEv

tableDivide :: Text
tableDivide = "divide-y divide-neutral-600"

card :: (DomBuilder t m, PostBuild t m) => (Map Text Text -> Dynamic t (Map Text Text)) -> m a -> m a
card mkAttrs = elDynAttr "div" $ mkAttrs $ "class" =: "h-full px-2 py-6 flex flex-col justify-between items-center gap-4 border border-neutral-600"

block :: DomBuilder t m => Text -> ((Text -> m ()) -> m a) -> m a
block layout contents = divClass blockClass $ contents lbl
  where
    blockClass = layout <> " gap-x-4 gap-y-1 bg-panel-block-bg rounded-lg p-4 text-neutral-400 text-xs my-2"
    lbl = elClass "span" "font-medium text-sm" . text


-- | Display an empty left panel saying no assets were found
emptyPanel :: DomBuilder t m => m ()
emptyPanel = do
  block "text-center italic" $ \lbl -> do
    el "p" $ do
      lbl "No assets in address."
      el "br" blank
      lbl "Please provision or select another."

addressSearch
  :: (DomBuilder t m, MonadHold t m, PostBuild t m)
  => Text
  -> Override (InputElementConfig EventResult t (DomBuilderSpace m))
  -> m (Dynamic t (Either Text PublicKey))
addressSearch lbl override = do
  block "grid" $ \mkLbl -> do
    let labelAttr = ("for" =: "to") <> ("class" =: "col-span-2")
    elAttr "label" labelAttr $ mkLbl lbl
    divClass "flex flex-row" $ do
      img $(static "img/icon_search.svg") $ pure $ \attrs -> attrs
        <> "alt" =: "Search Icon"
        <> "class" =: "mr-2"
      formField (Left "") parseAddress $ override . setPlaceholder "To"

deleteButton :: (DomBuilder t m, PostBuild t m) => Dynamic t Bool -> Override Text -> m () -> m (Event t ())
deleteButton enabled overrideStatic contents = do
  let commonClasses = overrideStatic "flex flex-row items-center rounded-sm font-medium"
      dynBtnAttrs = ffor enabled $ bool
        ("class" =: (commonClasses <> " " <> "bg-gray-700 cursor-not-allowed italic text-gray-400") <> "disabled" =: "")
        ("class" =: (commonClasses <> " " <> "bg-red-700" <> hover "bg-red-800"))
  (delBtn, _) <- elDynAttr' "button" dynBtnAttrs contents
  pure $ domEvent Click delBtn

trashIcon :: (DomBuilder t m, PostBuild t m) => m ()
trashIcon = img $(static "img/icon-trash.svg") $ pure $ \attrs -> fold
  [ attrs
  , "alt" =: "Delete icon"
  , "class" =: "h-6"
  ]

dialog_
  :: ( DomBuilder t m, MonadFix m, MonadHold t m, MonadJSM m, MonadJSM (Performable m), PerformEvent t m, PostBuild t m
     , ToJSVal (RawElement (DomBuilderSpace m))
     )
     => Event t (m ()) -> m (Event t ())
dialog_ = fmap void . dialog . fmap (never <$)

--TODO: place as immediate child of <body> ?
dialog
  :: ( DomBuilder t m, MonadFix m, MonadHold t m, MonadJSM m, MonadJSM (Performable m), PerformEvent t m, PostBuild t m
     , ToJSVal (RawElement (DomBuilderSpace m))
     )
  => Event t (m (Event t a)) -> m (Event t (Maybe a))
dialog showEv = mdo
  let
    mkDialog contents = divClass "flex flex-col justify-center items-center gap-6" $ do
      externalCloseEv <- contents
      internalCloseEv <- primaryButton "Close" pure
      pure $ leftmost
        [ Nothing <$ internalCloseEv
        , Just <$> externalCloseEv
        ]

  let dialogClass = "px-1 py-4 rounded-3xl bg-menu-bg text-white w-full text-xs tracking-tight text-wrap"
                 <> md "w-1/3 text-base px-8"
  (d, ((), dialogEv)) <- elClass' "dialog" dialogClass $ runWithReplace blank $ leftmost
    [ mkDialog <$> showEv
    , pure never <$ closeEv
    ]

  closeEv <- switchHold never dialogEv

  e <- liftJSM $ toJSVal (_element_raw d)


  let method name = void $ liftJSM $ e ^. js0 (name :: Text)

  performEvent_ $ leftmost
    [ method "showModal" <$ showEv
    , method "close" <$ closeEv
    ]

  pure closeEv

-- Dropdown menu to offer token selection from the set contained in the current wallet
tokenDropdown
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
  => Bool -> Dynamic t (Maybe (Map Token Amount)) -> m (Dynamic t (Either Text Token))
tokenDropdown defaultToFirst dynAssets = do
  divClass "flex gap-2 items-center" $ mdo
    -- Dynamic image of the selected token
    let
      renderImg = dynEither_ (const blank) $ \path -> img path $ pure $ \attrs -> attrs
        <> "class" =: "h-8"

    renderImg $ ffor dynToken $ fmap tokenIconPath

    pb <- getPostBuild
    let
      pickFirstOption mbs = note "" $ do
        bs <- mbs
        v <- Map.minViewWithKey bs
        pure $ fst $ fst v

      setValueEv = fmap pickFirstOption $ leftmost
        [ updated dynAssets
        , current dynAssets <@ pb
        ]

    -- Then the dropdown to select the token to send
    let dropdownClass = "rounded-lg bg-input-bg border-0 ring-0 text-neutral-200 tracking-tight font-semibold py-1"
                      <> focus "border-0 ring-1 ring-saline"
    val <- dropdown (Left "") (ffor dynAssets $ Map.mapKeysMonotonic Right . Map.mapWithKey (\t _ -> tshow t) . fromMaybe mempty) $ def
            & dropdownConfig_attributes .~ pure ("class" =: dropdownClass)
            & dropdownConfig_setValue .~ bool never setValueEv defaultToFirst
    let dynToken = _dropdown_value val
    pure dynToken

-- A number text input to enter the token amount ot transfer. Place holder text and max values
-- are dynamically adjusted based on the selected token
amountInput
  :: DomBuilder t m
  => Dynamic t (Maybe (Map Token Amount)) -> Event t () -> Dynamic t (Either Text Token) -> m (Dynamic t (Either Text Double))
amountInput dynAssets evMaxClicked dynToken = do
  let inputClassAttrs = "bg-transparent border-0 ring-0 text-neutral-200 text-sm font-semibold"
                      <> placeholder "italic"
                      <> focus "ring-0 border-b"
                      <> valid (focus "border-saline")
                      <> invalid "italic text-red-700 border-b border-red-700 font-normal"
                      <> oor "italic text-red-700 border-b border-red-700 font-normal"
      token_ = ffor dynToken $ fromMaybe BTC . hush
      amount_ = ffor2 token_ dynAssets $ \t -> Map.findWithDefault 0.0 t . fromMaybe mempty
      attrs = ffor (updated amount_) $ \maxAvail -> "placeholder" =: Just ("0 - " <> bool "..." (tshow maxAvail) (maxAvail > 0))
                                                <> "min" =: Just "0.0"
                                                <> "max" =: bool Nothing (Just (tshow maxAvail)) (maxAvail > 0)
                                                <> "step" =: Just ("0.000000001") -- needed so that float value are not considered invalid
  iE <- inputElement $ def
        & initialAttributes .~ ( "type" =: "number"
                              <> "class" =: inputClassAttrs)
        & modifyAttributes .~ attrs
        & inputElementConfig_setValue .~ (tshow <$> current amount_ <@ evMaxClicked)
  pure $ treadEither <$> _inputElement_value iE

type BalancesSummary = (Map Token Balance, Amount)

data Balance = Balance
  { _balance_inToken :: Amount
  , _balance_inUSD :: Amount
  , _balance_percentage :: Double
  } deriving stock Eq

balancesSummary :: Map Token Amount -> BalancesSummary
balancesSummary bs =
  let
    withUSD = Map.mapWithKey (\t b -> (b, toUSD t b)) bs
    total = sum $ fmap snd withUSD
  in
    (,total) $ ffor withUSD $ \(inToken, inUSD) -> Balance
      { _balance_inToken    = inToken
      , _balance_inUSD      = inUSD
      , _balance_percentage = inUSD / total * 100
      }

-- | In the Summary screen, display the owned tokens, their values and their proportional value
assetsTable
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => Bool -> [Text] -> Dynamic t BalancesSummary -> m ()
assetsTable displayIcons errSentences summary = do
  dynIsWalletNonEmpty <- holdUniqDyn $ not . Map.null . fst <$> summary
  -- Display a hint for the user that there is no balance to display because the wallet is empty
  let dispEmpty = divClass "text-2xl italic text-neutral-400 w-1/2 text-center" $ do
        traverse_ (el "p" . text) errSentences

  dyn_ $ ffor dynIsWalletNonEmpty $ bool dispEmpty $ do
    elAttr "table" ("class" =: "w-full font-light divide-y divide-neutral-600") $ do
      -- Set columns width (re-used, hence why in variable)
      let assetW = "w-2/12" <> md "w-3/12"
          balanceW = "w-3/12" <> md "w-2/12"
          divisionW = "w-7/12"
      -- Table headers
      elClass "tr" ("text-neutral-300 text-left text-xs ") $ do
        elClass "th" ("font-light text-left" <> " " <> assetW) $ text "Asset"
        elClass "th" ("font-light text-right" <> " " <> balanceW) $ text "Balance"
        elClass "th" ("font-light text-center" <> " " <> divisionW) $ text "Wallet Division"

      -- Filling the table
      void $ listWithKey (fmap fst summary) $ \t dynBalance -> do
        elClass "tr" ("h-12 text-xs" <> md "text-sm") $ do
          -- Token name / symbol (with a colored dot representing the token's color)
          elClass "td" ("text-left" <> " " <> assetW) $ do
            divClass "flex flex-row items-center" $ do
              if displayIcons then do
                let src = tokenIconPath t
                img (constDyn src) $ pure $ \attrs -> attrs
                  <> "class" =: "h-6 mr-2"
              else do
                let colorClass = "bg-[" <> tokenColor t <> "]"
                divClass ("rounded-full h-5 w-5 mr-2" <> " " <> colorClass) blank
              text (tshow t)
          -- Token balance ($-equiv and native)
          elClass "td" ("text-right font-medium tracking-wide" <> " " <> balanceW <> md "font-normal tracking-normal") $ do
            divClass "flex flex-col" $ do
              el "span" $ dynText $ ffor dynBalance $ ppUSDAmount True . _balance_inUSD
              elClass "span" ("text-neutral-500 italic") $ dynText $ ffor dynBalance $ parens . prettyPrintWithDecimals False 8 . _balance_inToken
          -- % of total wallet value
          elClass "td" ("text-center" <> " " <> divisionW) $ do
            let pctClass = "flex flex-row items-center pl-8"
                        <> md "pl-20"
            divClass pctClass $ do
              let dynPercentage = ffor dynBalance _balance_percentage
              dynText $ ffor dynPercentage $ (<> "%") . prettyPrintWithDecimals False 1

              -- Percentage bar container
              divClass "ml-2 flex flex-1 flex-row h-1" $ do
                -- Token percentage bar
                let constClasses = "bg-saline rounded-l"
                    dynClasses = fmap (\pct -> constClasses <> " w-[" <> tshow pct <> "%]") dynPercentage
                elDynClass "div" dynClasses blank
                -- Total percentage bar
                divClass "bg-neutral-700 rounded-r flex-1" blank

renderTxChain
  :: (DomBuilder t m, MonadJSM (Performable m), PerformEvent t m, PostBuild t m, RouteToUrl (R AppRoute) m, SetRoute t (R AppRoute) m, Prerender t m)
  => Int -> Maybe Int -> Height -> Maybe (Hash SignedTx) -> Base64String -> m ()
renderTxChain idx mNbTxInBlock blockHeight mHash tx' = case parseTx tx' of
  Left err -> divClass "font-bold text-red-600" $ text $ "Failed to decode transaction: " <> prettyPrintTxError err
  Right (Signed nonce signature tx _) -> renderTx (Just idx) mNbTxInBlock (Just blockHeight) mHash (Just signature) (Just nonce) tx

renderTxStandalone
  :: (DomBuilder t m, MonadJSM (Performable m), PerformEvent t m, PostBuild t m, RouteToUrl (R AppRoute) m, SetRoute t (R AppRoute) m, Prerender t m)
  => SignableTx -> m ()
renderTxStandalone = \case
  Left tx -> f Nothing Nothing tx
  Right (Signed nonce signature tx _) -> f (Just signature) (Just nonce) tx
  where
    f = renderTx Nothing Nothing Nothing Nothing

renderTx
  :: (DomBuilder t m, MonadJSM (Performable m), PerformEvent t m, PostBuild t m, RouteToUrl (R AppRoute) m, SetRoute t (R AppRoute) m, Prerender t m)
  => Maybe Int -> Maybe Int -> Maybe Height -> Maybe (Hash SignedTx) -> Maybe Signature -> Maybe Nonce -> Tx -> m ()
renderTx midx mNbTxInBlock mBlockHeight mHash mSignature mNonce tx = do
    let txClass = "text-xs bg-neutral-500 rounded my-3 p-2"
               <> lg "text-base"
    divClass txClass $ do
      -- Display Transaction hash (when we have it)
      for_ mHash $ \hash -> do
        divClass "flex flex-row h-6 gap-2 items-center" $ do
          let hashTxt = tshow hash
          divClass labelClass $ text "Tx hash:"
          elClass "pre" "text-xs truncate select-none cursor-default" $ text hashTxt
          (copyHashBtn, _) <- elClass' "button" copySigBtnClass
            $ img $(static "img/copy-icon.svg") $ pure $ \attrs -> attrs
              <> "alt"   =: "Copy hash"
              <> "title" =: "Copy hash to clipboard"
              <> "class" =: "h-full"
          copyToClipboard $ hashTxt <$ domEvent Mousedown copyHashBtn

      -- Display Tx position in block
      for_ midx $ \idx -> divClass "flex flex-row h-6 gap-2 items-center" $ do
        divClass labelClass $ text "Position:"
        el "div" $ do
          text $ tshow (1+idx)
          for_ mNbTxInBlock $ \nbTxInBlock -> do
            text $ "/" <> tshow nbTxInBlock

      -- Display the block the Tx belongs to
      for_ mBlockHeight $ \blockHeight -> divClass "flex flex-row h-6 gap-2 items-center" $ do
        divClass labelClass $ text "Block:"
        el "div" $ routeLink (AppRoute_Explorer ?/ ExplorerRoute_Block ?/ blockHeight ) $ do
          elClass "span" "text-saline font-medium py-3" $ do
            text (tshow blockHeight)
            elClass "span" "inline-block align-top text-xs" $ text "↗"

      -- Display Tx's signature and copy button
      for_ mSignature $ \signature -> divClass "flex flex-row h-6 gap-2 items-center" $ do
        let sig_ = prettyPrintSignature signature
        divClass labelClass $ text "Signature:"
        elClass "pre" "text-xs truncate select-none cursor-default" $ text (signatureEllipsis signature)
        (copySigBtn, _) <- elClass' "button" copySigBtnClass
          $ img $(static "img/copy-icon.svg") $ pure $ \attrs -> attrs
            <> "alt"   =: "Copy sig"
            <> "title" =: "Copy signature to clipboard"
            <> "class" =: "h-full"
        copyToClipboard $ sig_ <$ domEvent Mousedown copySigBtn

      -- Display Tx's nonce
      for_ mNonce $ \nonce -> divClass "flex flex-row h-6 gap-2 items-center" $ do
        divClass labelClass $ text "Nonce:"
        let preClass = "text-xs truncate tracking-tighter"
                    <> md "tracking-normal"
        elClass "pre" preClass $ text $ tshow nonce

      let transfers = flowPairs $ _tx_instructions tx
      case null transfers of
        True -> divClass "text-center" $ text "No balances changed"
        False -> do
          let transferClass = "flex flex-col gap-y-2 mt-4"
                           <> md "ml-20 mt-2"
          divClass transferClass $ renderTransfers transfers

  where
    -- Classes for the label in a tx display (things like "Nonce:", "Position:", etc.)
    labelClass = "flex-none w-14 font-medium"
              <> md "w-20"

    txLine = divClass "flex flex-row items-center place-content-center"

    -- Some common style to format an instruction, reused by different rendering widgets
    instrClass = "flex flex-row items-center text-xs bg-stone-300 text-stone-700 rounded pr-1"
              <> hover "bg-stone-200"
              <> lg "text-base"

    copySigBtnClass = "h-7 p-1" <> hover "bg-neutral-600 rounded cursor-copy"

    copyAddrBtnClass = "h-7 p-1 ml-1" <> hover "bg-stone-400 rounded cursor-copy"

    renderTransfers flows = ifor_ flows $ \(from', to') assets -> sequence_ $ ffor2 from' to' $ \from to ->
      renderOneTransfer from to assets

    renderOneTransfer from to assets = do
      -- Negative amount value are displayed on the left arrow (down) block from <from> to <to>
      let (forwardTokens, backwardTokens) = assets

      divClass instrClass $ do
        -- First column shows a custom icon and text for the transfer
        let fstColClass = "flex flex-col flex-none place-content-center"
                       <> " text-center w-16 h-full font-medium ml-1"
                       <> md "w-24"
                       <> lg "py-2"
        divClass fstColClass $ do
          img $(static "img/icon_transfer.svg") $ pure $ \attrs -> attrs
            <> "alt"   =: "transfer icon"
            <> "class" =: ("h-8" <> md "h-10")
          text $ Text.toUpper "Transfer"

        -- Second colum displays the information
        let sndColClass = "flex flex-col w-full h-full truncate p-1 pl-0"
                       <> lg "pl-5"
        divClass sndColClass $ do
          txLine $ do
            let from_ = addressFull from
            elClass "pre" "block text-xs truncate select-none cursor-default" $ text (addressEllipsis from)
            (copyFromBtn, _) <- elClass' "button" copyAddrBtnClass
              $ img $(static "img/copy-icon.svg") $ pure $ \attrs -> attrs
                <> "alt"   =: "Copy from address"
                <> "title" =: "Copy address to clipboard"
                <> "class" =: "h-full"
            copyToClipboard $ from_ <$ domEvent Mousedown copyFromBtn

          txLine $ do
            -- Container for the exchange blocks
            divClass "flex flex-row justify-evenly w-full" $ do
              unless (Map.null forwardTokens) $
                exchangeBlock Down forwardTokens

              unless (Map.null backwardTokens) $
                exchangeBlock Up backwardTokens

          txLine $ do
            let to_ = addressFull to
            elClass "pre" "text-xs truncate select-none cursor-default" $ text (addressEllipsis to)
            (copyToBtn, _) <- elClass' "button" copyAddrBtnClass $ do
              img $(static "img/copy-icon.svg") $ pure $ \attrs -> attrs
                <> "alt"   =: "Copy from address"
                <> "title" =: "Copy address to clipboard"
                <> "class" =: "h-full"
            copyToClipboard $ to_ <$ domEvent Mousedown copyToBtn

    exchangeBlock dir pairs = do
      divClass "flex flex-row w-1/2" $ do
        let arrowClass = "w-1/4"
                      <> md "w-1/3"
            currencyClass = "flex flex-col justify-evenly w-3/4"
                         <> md "w-2/3"
        divClass arrowClass $ do
          let src = if dir == Up then $(static "img/icon_up_arrow.svg") else $(static "img/icon_down_arrow.svg")
          elAttr "img" ("src" =: src <> "class" =: "h-full") blank
        divClass currencyClass $ do
          for_ (Map.toList pairs) $ \(token, amount) -> do
            let currencyLineCLass = "flex flex-row gap-x-1 items-center h-5 text-xs tracking-tighter"
                                 <> md "text-sm tracking-tight h-7"
            divClass currencyLineCLass $ do
              elAttr "img" ( "src" =: tokenIconPath token
                          <> "class" =: "h-full"
                          <> "title" =: tshow token
                          ) blank
              elClass "p" "text-center" $
                text $ (tshow amount) <> " " <> tshow token

-- | Simple type to draw the token exchange block in the right direction
data Dir = Up | Down
  deriving stock (Eq)

solver
  :: ( DomBuilder t m, MonadFix m, MonadHold t m, MonadJSM m, MonadJSM (Performable m), PerformEvent t m, Prerender t m, PostBuild t m
     , RouteToUrl (R AppRoute) m
     , SetRoute t (R AppRoute) m
     , ToJSVal (RawElement (DomBuilderSpace m))
     )
  => Env t -> SecretKey -> PublicKey -> Maybe Intent -> (Tx -> Tx) -> m ()
solver env sk host i overrideTx = do
  case nonEmpty $ trySolve (derivePublicKey sk) host mempty (fromMaybe (defaultIntent host) i) of
    Nothing -> text "Could not solve intent automatically"
    Just txs -> divClass "flex flex-col gap-8" $ for_ txs $ \(tx, signers) -> card pure $ do
      renderTxStandalone $ Left tx
      for_ (nonEmpty (toList signers)) $ \ss -> do
        divClass "flex flex-col items-center" $ do
          let isSingleSigner = signers == Set.singleton (derivePublicKey sk)
          bool errorMessage id isSingleSigner $ text "Required signers"
          for_ ss $ addressLink . pure

      let
        requiresExtraSigners = not $ null $ signers `Set.difference` Set.singleton (derivePublicKey sk)
        tx' = tx
          & disregardingInstalled (derivePublicKey sk)
          & overrideTx
      txButtons env "Submit" id $ pure $ Right $ (tx',) $ bool (Right sk) (Left "") requiresExtraSigners
