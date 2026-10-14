module Saline.Core.Validate where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import GHC.Natural

import Saline.Core.Types

import qualified Data.Map as Map

data ValidationError
    = ErrRemoteNoProof
    | ErrInvalidProof
    | ErrOwnerDidntSign
    | ErrIntentAlreadyInstalled
    | ErrIntentDoesntExist
    deriving stock Show

type V = Either ValidationError


todo :: a
todo = error "TODO" -- To shut up warnings

validateTx :: ChainState -> ChainTx -> V ChainState
validateTx s tx = case tx of
    TxGenesis gtx  -> validateGenesisTx s gtx
    TxProtocol ptx -> validateProtocolTx s ptx
    TxLedger ltx   -> validateLedgerTx s ltx
    TxCurrency ctx -> validateCurrencyTx s ctx
    TxBundle txs   -> foldM validateTx s txs

validateCurrencyTx :: ChainState -> Tx CurrencyTx -> V ChainState
validateCurrencyTx s _ = pure s

validateLedgerTx :: ChainState -> Tx LedgerTx -> V ChainState
validateLedgerTx s (Tx tx m) = case tx of
    TxIntent itx -> validateIntentTx s (Tx itx m)
    TxAssets atx -> validateAssetsTx s (Tx atx m)
    TxData   dtx -> validateDataTx   s (Tx dtx m)

validateDataTx :: ChainState -> Tx DataTx -> V ChainState
validateDataTx s _ = pure s

-- TODO: check for some special txs, eg somebody staking (= becoming a validator, to be added
-- to s ^. chainValidators)
validateAssetsTx :: ChainState -> Tx (TxShape AssetsTx) -> V ChainState
validateAssetsTx s (Tx atx meta) = case atx of
    -- TODO: lookup intents & verify, check balances, ...
    Prim t -> case t of
        TxSimple tx -> validateSimpleTx s (Tx tx meta)
        TxSwap tx -> validateSwapTx s (Tx tx meta)
    Gate as bs -> validateGateTx s (Tx (as, bs) meta)

validateGateTx :: ChainState -> Tx ([AssetsTx], [AssetsTx]) -> V ChainState
validateGateTx = todo

validateSwapTx :: ChainState -> Tx SwapTx -> V ChainState
validateSwapTx = todo

validateSimpleTx :: ChainState -> Tx SimpleTx -> V ChainState
validateSimpleTx = todo

-- for now assumes owner needs to sign intent installation
-- later: intent on intent updates
-- TODO: should we allow an option to "commit" funds to an intent? preventing
-- any other use than for this intent? sounds appealing in terms of guarantees.
validateIntentTx :: ChainState -> Tx IntentTx -> V ChainState
validateIntentTx s (Tx (InstallIntent addr _assets intent mproof) m) = do
    checkOwnerSigned addr m -- until "intents for intent updates"
    checkNotAlreadyInstalled addr intent s
    case addr of
        Remote _ -> case mproof of
            Nothing -> throwError ErrRemoteNoProof
            Just p  -> if verifyProof p then pure () else throwError ErrInvalidProof
        _ -> pure ()
    -- TODO: onboard 'assets' when remote, commit when local?
    pure $ installIntent addr intent s
validateIntentTx s (Tx (RemoveIntent addr intent) m) = do
    checkOwnerSigned addr m
    checkIntentExists addr intent s
    pure $ removeIntent addr intent s

validateProtocolTx :: ChainState -> Tx ProtocolTx -> V ChainState
validateProtocolTx s (Tx ptx _) = pure $ case ptx of
    -- TODO: check some stuffs, don't just accept...
    ProtocolTxUpgrade v d -> s & chainNextProto .~ Just (v, d)
    ProtocolTxUpgradeNow v -> s & chainProto .~ v

validateGenesisTx :: ChainState -> Tx GenesisTx -> V ChainState
validateGenesisTx s gtx = pure $
    -- TODO: check some stuffs, don't just accept...
    s & chainProto .~ (gtx ^. txData.genesisVersion)

checkOwnerSigned :: Addr -> TxMeta -> V ()
checkOwnerSigned addr m
    | any (\k -> pubKeyOwns k addr) (m ^. metaSigners) = pure ()
    | otherwise = throwError ErrOwnerDidntSign

checkNotAlreadyInstalled :: Addr -> Intent -> ChainState -> V ()
checkNotAlreadyInstalled addr i s
    | i `notElem` s ^. chainLedger.ledgerIntents.ix addr = pure ()
    | otherwise = throwError ErrIntentAlreadyInstalled

checkIntentExists :: Addr -> Natural -> ChainState -> V ()
checkIntentExists addr i s = case Map.lookup addr (s ^. chainLedger.ledgerIntents) of
    Just is | i < fromIntegral (length is) -> pure ()
    _ -> throwError ErrIntentDoesntExist

installIntent :: Addr -> Intent -> ChainState -> ChainState
installIntent addr i s = s & chainLedger.ledgerIntents %~ Map.insertWith (++) addr [i]

removeIntent :: Addr -> Natural -> ChainState -> ChainState
removeIntent addr i s = s & chainLedger.ledgerIntents.at addr %~ fmap (removeN 0)
    where removeN :: Natural -> [a] -> [a]
          removeN _ [] = []
          removeN k (x:xs)
            | i == k = xs
            | otherwise = x : removeN (k+1) xs
