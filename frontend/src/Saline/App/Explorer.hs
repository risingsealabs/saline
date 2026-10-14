{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Saline.App.Explorer where

import Control.Lens (ifor_)
import Control.Monad (when, (<=<))
import Control.Monad.Fix
import Data.Bool (bool)
import Data.ByteArray.Base64String (Base64String (unBase64String))
import Data.ByteString qualified as ByteString
import Data.Foldable (for_)
import Data.Functor (void)
import Language.Javascript.JSaddle (MonadJSM)
import Network.ABCI.Types.Messages.FieldTypes (BlockID (blockIDHash), Header (..), WrappedVal (unWrappedVal))
import Network.Tendermint.Client (Block (..), BlockMeta (..), Data (dataTxs), EvidenceData (..), ResultBlock (resultBlockBlock, resultBlockBlockMeta), ResultTx (..))
import Obelisk.Generated.Static (static)
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Saline.App.Assets (img)
import Saline.App.Editor.PrettyPrint (ppHexString, ppWithThousands)
import Saline.App.Editor.Utils.Reflex
import Saline.App.Environment
import Saline.App.Tailwind
import Saline.App.Widgets
import Saline.Crypto (textEllipsis)
import Saline.Interface.Query
import Saline.Interface.Route
import Saline.Network.Node.RPC qualified as RPC
import Saline.Network.Node.Transaction
import Saline.Utils

explorer
  :: ( DomBuilder t m, MonadFix m, MonadHold t m, MonadJSM (Performable m), PerformEvent t m, PostBuild t m, Prerender t m
     , RouteToUrl (R AppRoute) m
     , SetRoute t (R AppRoute) m
     )
  => Env t
  -> RoutedT t (Maybe (R ExplorerRoute)) m ()
explorer env = do
  let explorerScreenClass = "flex flex-col items-center p-1"
                          <> md "p-8"
                          <> lg "p-16"
  divClass explorerScreenClass $ do
    maybeRoute_ blank $ subRoute_ $ \case

      ExplorerRoute_Address -> addressDetails env =<< askRoute

      ExplorerRoute_Block -> do
        r <- askRoute
        res <- holdDyn Nothing . fmap Just <=< rpcDyn env $ ffor r RPC.block

        let blockDetailsClass = "w-full bg-[#2a2a31] rounded px-4 py-2"
                              <> lg "w-10/12"
                              <> xl "w-6/12"
        void $ divClass blockDetailsClass $ do
          elClass "h3" "text-center font-medium text-lg pb-1 border-b border-neutral-600" $ text "Block Info"
          let renderErr = divClass "flex flex-col items-center" $ text "Could not find block"
          let render = dynLoading_ $ dynEither_ (\_ -> renderErr) $ dyn_ . fmap (blockDetails r)
          render res

      ExplorerRoute_Tx -> dynRoute_ $ \hash -> do
        res <- holdDyn Nothing . fmap Just <=< rpcDyn env $ pure $ RPC.transaction hash
        let render = dynLoading_ $ dynEither_ (\_ -> text "Could not find transaction") $ dyn_ . fmap (transactionDetails hash)
        void $ divClass "flex flex-col items-center" $ render res

      ExplorerRoute_Intents -> do
        resp <- pollDyn env SalineQuery_Intents $ pure ()
        intentsGrid resp $ \mkCard i hosts -> do
          void $ listWithKey hosts $ \pk _ -> mkCard $ do
            intent $ Right i
            ev <- formButton "Fulfill" (pure True) id
            setRoute $ ffor ev $ \() -> AppRoute_Intents :/ IntentsRoute_Fulfill ?/ pk

transactionDetails
  :: forall t m. (DomBuilder t m, RouteToUrl (R AppRoute) m, SetRoute t (R AppRoute) m, Prerender t m, MonadJSM (Performable m), PerformEvent t m, PostBuild t m)
  => Hash SignedTx -> ResultTx -> m ()
transactionDetails hash res = do
  let
    txIdx = fromIntegral $ resultTxIndex res
    tx = resultTxTx res
    blockHeight = fromIntegral . unWrappedVal . resultTxHeight $ res

  renderTxChain txIdx Nothing blockHeight (Just hash) tx

blockDetails
  :: forall t m
  . (DomBuilder t m, RouteToUrl (R AppRoute) m, SetRoute t (R AppRoute) m, Prerender t m, MonadHold t m, MonadFix m, PostBuild t m, MonadJSM (Performable m), PerformEvent t m)
  => Dynamic t (Maybe Height) -> ResultBlock -> m ()
blockDetails mHeight b = do
  let
    blockHash = blockIDHash . blockMetaBlockId . resultBlockBlockMeta $ b
    header = blockMetaHeader . resultBlockBlockMeta $ b
    nbBlockTx = unWrappedVal . headerNumTxs $ header
    proposerAddr = headerProposerAddress header
    mParentHash = blockIDHash <$> headerLastBlockId header
    txs = unWrappedVal . dataTxs . blockData . resultBlockBlock $ b
    nbBytes = sum $ fmap (ByteString.length . unBase64String) txs
    offenses = unWrappedVal . evidenceDataEvidence . blockEvidence . resultBlockBlock $ b
    h = fromIntegral . unWrappedVal . headerHeight $ header

  -- Display the block's detail
  let gridClass = "flex flex-col items-center text-sm leading-relaxed" -- labels and data on top of each other on mobile
               <> md "text-base grid grid-cols-3" -- Make it a grid on larger screens
               <> lg "text-lg leading-loose"
  divClass gridClass $ do
    -- Display the block height, with optional arrows to go to the previous or next one
    section "Block height:" $ do
      divClass "flex gap-2 items-center" $ do
        text $ tshow h
        let arrowClass = "flex items-center place-content-center h-5 w-5 bg-saline rounded-md"
                      <> hover "bg-saline-light"

        -- Only show show a link to the parent block if this is not the first block (debug: block 2 for now)
        when (h > 2) $ do
          elClass "button" arrowClass $ do
            routeLink (AppRoute_Explorer ?/ ExplorerRoute_Block :/ Just (h - 1) ) $ text "<"

        let displayFollowHead = elClass "span" "text-xs italic tracking-tight text-neutral-500"
              $ text "(following blockchain's head)"

            goToHeadBtn _ = do
              -- Display "next" button
              elClass "button" arrowClass $ do
                routeLink (AppRoute_Explorer ?/ ExplorerRoute_Block :/ Just (h + 1) ) $ text ">"
              -- Display "follow head" button
              elClass "button" arrowClass $ do
                routeLink (AppRoute_Explorer ?/ ExplorerRoute_Block :/ Nothing ) $ text ">>"

        dynMaybe_ displayFollowHead goToHeadBtn mHeight

        blank

    -- Display the block hash
    section "Hash:" $ do
      divClass "flex flex-row items-center gap-x-1" $ do
        let blockHash_ = ppHexString blockHash
        pre $ text $ textEllipsis blockHash_ "0x" 15
        (copyBlockHashBtn, _) <- elClass' "button" copyHashBtnClass
          $ img $(static "img/copy-icon.svg") $ pure $ \attrs -> attrs
            <> "alt"   =: "Copy hash"
            <> "title" =: "Copy hash to clipboard"
            <> "class" =: "h-full"
        copyToClipboard $ blockHash_ <$ domEvent Mousedown copyBlockHashBtn


    -- Display the number of transactions in the block (and a link to inspect all transactions in this block)
    section "Transactions:" $ do
      elClass "span" ("text-saline underline-offset-4") $ text $ simplePlural nbBlockTx "transaction"
      text " in this block"

    -- Display the size in bytes of the block
    section "Size" $ do
      -- This is just to get the number pretty printed with thousands separator (eg: 12,345 bytes)
      text $ ppWithThousands nbBytes <> " bytes"

    -- Display the block proposer's address
    section "Proposer:" $ do
      divClass "flex flex-row items-center gap-x-1" $ do
        let proposerAddr_ = ppHexString proposerAddr
        pre $ text $ textEllipsis proposerAddr_ "0x" 15
        (copyProposerBtn, _) <- elClass' "button" copyHashBtnClass
          $ img $(static "img/copy-icon.svg") $ pure $ \attrs -> attrs
            <> "alt"   =: "Copy proposer"
            <> "title" =: "Copy proposer to clipboard"
            <> "class" =: "h-full"
        copyToClipboard $ proposerAddr_ <$ domEvent Mousedown copyProposerBtn

    -- Display the hash of the parent block if any
    for_ mParentHash $ \parentHash -> do
      section "Parent hash:" $ do
        divClass "flex flex-row items-center gap-x-1" $ do
          let parentHash_ = ppHexString parentHash
          pre $ text $ textEllipsis parentHash_ "0x" 15
          (copyParentHashBtn, _) <- elClass' "button" copyHashBtnClass
            $ img $(static "img/copy-icon.svg") $ pure $ \attrs -> attrs
              <> "alt"   =: "Copy parent hash"
              <> "title" =: "Copy parent hash to clipboard"
              <> "class" =: "h-full"
          copyToClipboard $ parentHash_ <$ domEvent Mousedown copyParentHashBtn

    -- | Display the number of offenses (EvidenceData)
    section "Offences:" $ do
      let offensesClass = if (null offenses) then "italic font-light text-xs" else "text-red-600 font-medium"
      elClass "span" offensesClass $ do
        text $ simplePlural (Prelude.length offenses) "offense" <> " for this block"
        if (null offenses) then
          elClass "span" "text-green-600 text-lg px-1" $ text "✓"
        else
          elClass "span" "text-orange-600 text-lg font-semibold px-1" $ text "⚠"

  -- Display a title-that-is-a-button to toggle displaying or not the transactions in that block
  let toggleBtnClass = "text-center font-medium text-lg pb-1 border-b border-neutral-600 mt-8"
                     <> bool " cursor-pointer" "" (null txs)
  rec
    (elTitle, _) <- elClass' "h4" toggleBtnClass
     $ do
      dynText dynTitle
      text $ " (" <> tshow (length txs) <> ")"

    dynDisplayTx <- toggle True $ domEvent Click elTitle
    let dynTitle = ffor dynDisplayTx $ \disp -> do
          let transactionTxt = "Transactions "
          if null txs
            then transactionTxt
            else bool "▼ " "▲ " disp <> transactionTxt

  let dynTxListClass = ffor dynDisplayTx $ bool
       "hidden"
       "visible text-sm"

  -- | Display block's transactions
  elDynClass "div" dynTxListClass $ do
    ifor_ txs $ \idx tx -> renderTxChain idx (Just (length txs)) h Nothing tx

  where
    pre = elClass "pre" "text-xs overflow-x-auto select-none cursor-default"

    copyHashBtnClass = "h-7 p-1" <> hover "bg-neutral-600 rounded cursor-copy"

    section label data_ = do
      let headerClass = "text-neutral-400 text-base"
    -- Default style of the second column, the "data"
          dataClass = "text-neutral-200 text-sm mb-3"
                     <> md "text-base col-span-2" -- only on larger screen this is a grid
      divClass headerClass $ text label
      divClass dataClass data_
