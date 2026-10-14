{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Saline.Network.Node.Handlers where

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative(liftA2)
#endif
import Control.Arrow ((&&&))
import Control.Error.Util (hoistEither)
import Control.Exception (SomeException (..), catch)
import Control.Lens (ifor, ifor_, strict, over, view, (%~), (.~), (^.))
import Control.Monad (guard, when, (<=<), unless)
import Control.Monad.Except (ExceptT (..), MonadError (throwError), runExceptT, withExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State (StateT, execStateT, get, gets, modify)
import Control.Monad.Trans (lift)
import Data.Aeson (ToJSON)
import Data.Aeson qualified as Aeson
import Data.Bifunctor
import Data.ByteArray.Base64String (Base64String (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Conduit.Network (serverSettings)
import Data.Containers.ListUtils (nubOrd)
import Data.Default (Default (def))
import Data.Foldable (for_, toList, asum)
import Data.Function (fix, (&))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time
import Data.Tuple (swap)
import Network.ABCI.Server (serveAppWith)
import Network.ABCI.Server.App
import Network.ABCI.Types.Messages.FieldTypes qualified as ABCI
import Network.ABCI.Types.Messages.Request qualified as Request
import Network.ABCI.Types.Messages.Response qualified as Response
import Obelisk.Configs (runConfigsT)
import Obelisk.ExecutableConfig.Lookup (getConfigs)
import Obelisk.Route (R, tryDecode, pattern (:/))
import System.Console.ANSI
import Witherable (Filterable (catMaybes), mapMaybe)

import Saline.Config (getConfigFeedbackLogs, withWatcherConfig)
import Saline.Crypto
import Saline.Intents (Contracts, Intent, Wallet)
import Saline.Intents qualified as Intents
import Saline.Intents.Condition
import Saline.Intents.Instrument (faucetTap)
import Saline.Intents.Witness
import Saline.Interface.Query
import Saline.Network.CometBFT.Config
import Saline.Network.CometBFT.Node
import Saline.Network.Node.Storage
import Saline.Network.Node.Testnet (faucetAccount)
import Saline.Network.Node.Transaction
import Saline.Utils

type Handler = ExceptT TxError IO

--TODO: dev/test-only: make unreachable from mainnet codepaths
feedFaucet :: Monad m => StateT Store m ()
feedFaucet = ifor_ faucetTap $ \t a -> mint (derivePublicKey faucetAccount) t a

runABCIDev :: SalineEncoder -> Store -> CometBFTNode -> IO ()
runABCIDev enc genesis n = do
  cfgs <- getConfigs
  let path = "common/watcher"
  runConfigsT cfgs $ withWatcherConfig path (Map.lookup path cfgs) getConfigFeedbackLogs $ \watcher ->
    liftIO $ runABCIWithConfigs enc genesis n watcher feedFaucet

runABCIExe :: SalineEncoder -> Store -> FilePath -> CometBFTNode -> IO ()
runABCIExe enc genesis watcherConfigPath n = do
  --TODO: can ByteString.readFile throw anything else besides 'not found' ?
  contents <- fmap Just (ByteString.readFile watcherConfigPath) `catch` (\(SomeException _) -> pure Nothing)
  withWatcherConfig (Text.pack watcherConfigPath) contents getConfigFeedbackLogs $ \watcher ->
    liftIO $ runABCIWithConfigs enc genesis n watcher feedFaucet

runABCITest
  :: TrustedWatcher -> SalineEncoder -> Store -> CometBFTNode -> IO ()
runABCITest watcher enc genesis n = runABCIWithConfigs enc genesis n watcher (return ())

runABCIWithConfigs :: SalineEncoder -> Store -> CometBFTNode -> TrustedWatcher -> StateT Store Handler () -> IO ()
runABCIWithConfigs enc genesis n watcher onBlockStart = do
  let
    (host, port) = unsafeHostPortFromURI $ n ^. cometBFTNode_config ^. config_proxyApp

    transformHandler :: Handler (Response t) -> IO (Response t)
    transformHandler er = runExceptT er >>= \case
      Right r -> pure r
      Left l ->
        pure $ ResponseException $
          def & Response._exceptionError .~ tshow l

    serve = serveAppWith (serverSettings (fromEnum port) (fromString $ Text.unpack host)) mempty

  s <- newIORef genesis

  serve $ transformApp transformHandler $ App $ \case
    RequestEcho _ -> pure def
    RequestFlush _ -> pure def
    RequestInfo _ -> pure def
    RequestSetOption _ -> pure def
    RequestInitChain _ -> pure def
    RequestQuery q -> query enc s q
    RequestBeginBlock b -> beginBlock onBlockStart b s
    RequestCheckTx (Request.CheckTx hx _hty) -> check s watcher hx
    RequestDeliverTx (Request.DeliverTx hx) -> deliver s watcher hx
    RequestEndBlock _ -> pure def
    RequestCommit _ -> pure def

abciEnv :: Text -> ThreadEnv
abciEnv home = ThreadEnv
  { _threadEnv_component = "ABCI " <> home
  , _threadEnv_color = Green
  }

query :: SalineEncoder -> IORef Store -> Request.Query -> Handler (Response 'MTQuery)
query enc ref q = fmap (either reject accept) $ runExceptT $ do
  let
    ea = do
      txt <- first tshow $ Text.decodeUtf8' $ unBase64String $ Request.queryData q
      dat <- hex2bytestring txt
      tryDecode enc (Request.queryPath q, dat)

  case ea of
    Left err -> throwError err
    Right (CometQuery_Store :/ sq) -> liftIO $ run sq
  where
    --TODO: de-dup?
    run :: R SalineQuery -> IO ByteString
    run = route $ \case
      SalineQuery_Intent    -> qq SalineQuery_Intent
      SalineQuery_Intents   -> qq SalineQuery_Intents
      SalineQuery_Contracts -> qq SalineQuery_Contracts
      SalineQuery_ForSale   -> qq SalineQuery_ForSale
      SalineQuery_Duties    -> qq SalineQuery_Duties
      SalineQuery_Rights    -> qq SalineQuery_Rights
      SalineQuery_Balance   -> qq SalineQuery_Balance
      SalineQuery_Wallet    -> qq SalineQuery_Wallet
      SalineQuery_Aggregate -> qq SalineQuery_Aggregate
      SalineQuery_Swaps     -> qq SalineQuery_Swaps

    qq :: ToJSON b => SalineQuery (Req a b) -> Req a b -> IO ByteString
    qq sq (Req a) = fmap (view strict . Aeson.encode) . query' ref sq $ a

    accept v = ResponseQuery $ def
      & Response._queryValue .~ Base64String v

    reject n = ResponseQuery $ def
      & Response._queryCode .~ 1
      & Response._queryValue .~ Base64String (Text.encodeUtf8 n)

--TODO: make sure error on thrown on IO are caught
query' :: IORef Store -> SalineQuery (Req a b) -> a -> IO b
query' ref q a = do
  s <- liftIO $ readIORef ref
  (\f -> f a s) $ case q of
    SalineQuery_Intent -> \addr -> pure . getIntent addr
    SalineQuery_Intents -> \() -> pure . getIntents
    SalineQuery_Contracts -> \() -> pure . getContracts
    SalineQuery_ForSale -> \pk -> pure . getForSale pk
    SalineQuery_Duties -> \pk -> pure . getDuties pk
    SalineQuery_Rights -> \pk -> pure . getRights pk
    SalineQuery_Balance -> \(addr, token) -> pure . getBalance addr token
    SalineQuery_Wallet -> \addr -> pure . getWallet addr
    SalineQuery_Aggregate -> \keys -> pure . getAggregateBalances keys
    SalineQuery_Swaps -> \(counterparty, flows) -> pure . getMatchingSwaps counterparty flows

initChain :: Request.InitChain -> IORef Store -> Handler (Response 'MTInitChain)
initChain req ref = do
  s <- liftIO $ readIORef ref
  s' <- flip execStateT s $ for_ (Request.initChainTime req) $ \(ABCI.Timestamp ts) -> updateClock ts
  liftIO $ writeIORef ref s'

  pure def

beginBlock :: StateT Store Handler () -> Request.BeginBlock -> IORef Store -> Handler (Response 'MTBeginBlock)
beginBlock onBlockStart req ref = do
  s <- liftIO $ readIORef ref

  --TODO: instead of going through all intents, keep track of a deletion queue
  s' <- flip execStateT s $ do
    for_ (ABCI.headerTime =<< Request.beginBlockHeader req) $ \(ABCI.Timestamp ts) -> do
      updateClock ts

    ts <- gets _store_timestamp

    ifor_ (_store_intents s) $ \pk _ -> do
      modify $ restrictLimitation Nothing (Just $ tick ts) pk

    modify $ over store_intents $ fmap $ resolveOracle (_store_oracles s) ts
    modify $ over store_contracts $ Map.mapKeysMonotonic $ resolveOracle (_store_oracles s) ts --TODO: is this really monotonic?

    onBlockStart

  liftIO $ writeIORef ref s'

  pure def

check :: IORef Store -> TrustedWatcher -> Base64String -> Handler (Response 'MTCheckTx)
check ref watcher hx = withTx "CheckTx:" ref watcher hx reject $ \_ -> pure def
  where
    reject err = pure $ ResponseCheckTx $ def
      & Response._checkTxCode .~ 1
      & Response._checkTxData .~ txErrorToTxData err

deliver :: IORef Store -> TrustedWatcher -> Base64String -> Handler (Response 'MTDeliverTx)
deliver ref watcher hx = withTx "DeliverTx:" ref watcher hx reject $ \s -> do
  liftIO $ writeIORef ref s
  pure def
    where
      reject err = pure $ ResponseDeliverTx $ def
        & Response._deliverTxCode .~ 1
        & Response._deliverTxData .~ txErrorToTxData err

deliver' :: IORef Store -> TrustedWatcher -> SignedTx -> ExceptT TxError IO ()
deliver' ref watcher stx = withTx' "DeliverTx:" ref watcher stx $ liftIO . writeIORef ref

withTx
  :: MonadIO m
  => Text -> IORef Store -> TrustedWatcher -> Base64String -> (TxError -> m a) -> (Store -> m a) -> m a
withTx _lbl ref watcher hx reject persist = do
  either reject pure <=< runExceptT $ do
    stx <- hoistEither $ parseVerifyTx hx
    withTx' _lbl ref watcher stx persist

withTx'
  :: MonadIO m
  => Text -> IORef Store -> TrustedWatcher -> SignedTx -> (Store -> m a) -> ExceptT TxError m a
withTx' _lbl ref watcher stx persist = do
  let
    signers = _signed_signers stx
    submitter = NonEmpty.head signers --TODO: make submitter more explicit
    tx = _signed_signee stx
    instructions = _tx_instructions tx
    masks = _tx_intentMask tx

--  liftIO $ putStrLn $ Text.unpack $ Text.decodeUtf8 $ view strict $ Aeson.encode stx

  s <- liftIO $ readIORef ref

  ifor_ masks $ \pk m -> case m of
    Left _ -> pure ()   --TODO: verify/consume
    Right _ -> withExceptT TxError_Execution $ assertSignedBy signers pk $ pure ()

  let
    flows = assetFlows instructions
    participants = Map.keys flows

    disallow = fmap catMaybes $ ifor flows $ \pk f ->
      let
        mask = do
          Map.lookup pk masks >>= \case
            Left (holder, m) -> do
              guard $ contractInEffect pk holder m s
              Just m
            Right m -> Just m

        i = fromMaybe (defaultIntent pk) $ asum
          [ mask
          , getIntent pk s
          ]
        w = fromMaybe Witness_Auto (Map.lookup pk $ _tx_witnesses tx)

        addHolder = insertValue $ Var VariableTag_Address holderVariableName
        args = maybe id (\_ -> addHolder submitter) mask $ _tx_arguments tx

        balances = getBalances pk s

      in
        verifyFlow balances args signers f w i

    bail = throwError . TxError_Execution . AppError_Intent . VerificationError_Disallowed

  case disallow of
    Right failures -> unless (null failures) $ bail disallow
    _ -> throwError . TxError_Execution . AppError_Intent . VerificationError_Disallowed $ disallow
  let
    execution = do
      for_ participants $ modify . restrictLimitation (Just consumeUse) Nothing
      for_ instructions $ runInstruction signers watcher
      for_ participants $ \p -> do
        s' <- get
        ifor_ (getBalances p s') $ \t b ->
          when (b < 0) $ throwError $ AppError_InsufficientFunds p t b

  case execStateT execution s of
    Left err -> do
      liftIO $ print err
      throwError $ TxError_Execution err
    Right s' -> lift $ persist s'

runInstruction :: NonEmpty PublicKey -> TrustedWatcher -> Instruction -> StateT Store (Either AppError) ()
runInstruction signers watcher = \case

  -- TODO: More unified story with other instructions?
  -- Currently only transfers are verified against intents and all at once
  Instruction_Funds from to assets -> ifor_ assets $ curry $ transfer (from, to)

  Instruction_OrIntent owner intent -> intentGovernance owner $ orIntent owner intent

  Instruction_SetIntent owner intent -> intentGovernance owner $ setIntent owner intent

  Instruction_Delete owner -> intentGovernance owner $ delete owner

  Instruction_Bridge account tx -> case tx of
    BridgeInstruction_Burn t a -> assertSignedBy signers account $ burn account t a
    BridgeInstruction_Mint proof t a -> case (proof == watcher) of
      True -> mint account t a
      False -> throwError AppError_Untrusted

  where
    intentGovernance owner x = assertSignedBy signers owner $ do
      s <- get
      for_ (getIntent owner s) $ \i ->
        when (isLocked i) $ throwError $ AppError_Intent VerificationError_Locked
      x

assertSignedBy :: MonadError AppError m => NonEmpty PublicKey -> PublicKey -> m a -> m a
assertSignedBy signers signer act = case signer `elem` signers of
  False -> throwError $ AppError_Unsigned signer
  True -> act

getWallet :: PublicKey -> Store -> Wallet
getWallet addr = getBalances addr &&& getIntent addr

getAggregateBalances:: Set PublicKey -> Store -> Map Token Amount
getAggregateBalances keys store =
  let
    ks = toList keys
    maps = fmap (\key -> getBalances key store) ks
  in Map.unionsWith (+) maps

getBalances :: PublicKey -> Store -> Map Token Amount
getBalances addr = fromMaybe mempty . Map.lookup addr . _store_balances

getBalance :: PublicKey -> Token -> Store -> Amount
getBalance addr t = fromMaybe 0 . Map.lookup t . getBalances addr

nonEmptyFoldable :: Foldable t => t a -> Maybe (t a)
nonEmptyFoldable m = case null m of
  False -> Just m
  True -> Nothing

nonZero :: (Eq a, Num a) => a -> Maybe a
nonZero = \case
  0 -> Nothing
  x -> Just x

modifyBalance :: Token -> PublicKey -> (Amount -> Amount) -> (Store -> Store)
modifyBalance token addr f = store_balances %~ Map.alter (nonEmptyFoldable . mapMaybe nonZero . maybe (Map.singleton token (f 0)) upsert) addr
  where
    upsert = Map.alter (Just . f . fromMaybe 0) token

--TODO: avoid brute force
--TODO: generalise
getMatchingSwaps :: PublicKey -> (Token, Token) -> Store -> Map (Trade Amount) (NonEmpty Tx)
getMatchingSwaps counterparty (fw, bw) s = do
  Map.fromListWith (<>) $ matches =<< Map.toList (_store_intents s)
  where
    matches (host, i) =
      let solutions = trySolve counterparty host mempty i
      in flip mapMaybe solutions $ \(tx, _) -> do

        --TODO: fragile, how to keep in sync with solver?
        flows <- case _tx_instructions tx of
          Instruction_Funds _ _ flows :| [] -> pure flows
          _ -> Nothing

        guard $ Map.keysSet flows == Set.fromList [fw, bw]
        f <- Map.lookup fw flows
        b <- Map.lookup bw flows

        guard $ f > 0 && b < 0 --TODO: shouldn't be needed here

        pure (Trade (f, negate b), tx :| [])

--TODO: avoid brute force
getForSale :: PublicKey -> Store -> Contracts (Map PublicKey (Word, NonEmpty Tx))
getForSale counterparty s = Map.mapMaybeWithKey matches $ reverseLookup $ _store_intents s
  where
    matches i hosts = nonEmptyFoldable $ catMaybes $ flip Map.fromSet hosts $ \h ->
      let
        issuers = flip mapMaybe (_store_contracts s) $ nonEmpty . nubOrd . fmap fst . Map.keys
        solutions = trySolve counterparty h issuers i
        options = flip mapMaybe solutions $ \(tx, _) -> pure tx
      in
        flip fmap (nonEmpty options) $ \opts ->
          (toEnum $ length opts, opts)

--TODO: propagate simplification/deletion when resulting _Any becomes unsatisfiable?
isLocked :: Intent -> Bool
isLocked = fix $ \go -> \case
  Condition_All cs -> any go cs
  Condition_Any _ cs -> any go cs
  Condition_Counterparty {} -> False
  Condition_Restriction {} -> False
  Condition_Signature {} -> False
  Condition_Finite _ c -> go c
  Condition_Temporary _ _ _ -> True

--TODO: propagate simplification/deletion when resulting _Any becomes unsatisfiable?
restrictLimitation
  :: Maybe (Uses -> Intent -> Maybe Intent)
  -> Maybe ((Duration, Bool) -> Intent -> Maybe Intent)
  -> PublicKey
  -> (Store -> Store)
restrictLimitation restrictUses restrictTemporary pk = store_intents %~ Map.alter (>>= go) pk
  where
    -- TODO: cleanup
    go this = case this of
      Condition_All cs -> Condition_All <$> traverse go cs
      Condition_Any k cs -> Just $ Condition_Any k $ mapMaybe go cs
      Condition_Counterparty {} -> Just this
      Condition_Restriction {} -> Just this
      Condition_Signature {} -> Just this
      Condition_Finite n c -> maybe (Just this) f restrictUses
        where f g = g n =<< go c
      Condition_Temporary d a c -> maybe (Just this) f restrictTemporary
        where f g = g (d, a) =<< go c

consumeUse :: Uses -> Intent -> Maybe Intent
consumeUse n i = case n of
  1 -> Nothing
  0 -> Nothing --TODO: can we remove 0 values alltogether?
  _ -> Just $ Condition_Finite (pred n) i

tick :: DiffTime -> (Duration, Bool) -> Intent -> Maybe Intent
tick currentTime l@(ts, availableAfter) i = case availableAfter of
  False -> case currentTime > ts of
    False -> Just $ uncurry Condition_Temporary l i
    True -> Nothing
  True  -> case currentTime < ts of
    False -> Just i
    True -> Just $ uncurry Condition_Temporary l i

--TODO: propagate simplification/deletion when resulting _Any becomes unsatisfiable?
resolveOracle :: Map PublicKey VariableValues -> Timestamp -> Intent -> Intent
resolveOracle _ _ = oracle
  where
    oracle = fix $ \go this -> case this of
      Condition_All cs -> Condition_All $ fmap go cs
      Condition_Any k cs -> Condition_Any k $ fmap go cs
      Condition_Counterparty {} -> this
      Condition_Restriction r -> Condition_Restriction $ fmap expr r
      Condition_Signature {} -> this
      Condition_Finite {} -> this
      Condition_Temporary {} -> this

    expr this = case this of
      _ -> this

getIntents :: Store -> Contracts (Map PublicKey ())
getIntents = fmap (Map.fromSet $ const ()) . reverseLookup . _store_intents

getIntent :: PublicKey -> Store -> Maybe Intent
getIntent addr = Map.lookup addr . _store_intents

contractInEffect :: PublicKey -> PublicKey -> Intent -> Store -> Bool
contractInEffect issuer holder i = isJust . (Map.lookup (issuer, holder) <=< Map.lookup i) . _store_contracts

getContracts :: Store -> Contracts (Map (PublicKey, PublicKey) Word)
getContracts = _store_contracts

getDuties, getRights :: PublicKey -> Store -> Contracts (Map PublicKey Word)
(getDuties, getRights) = (project id, project swap)
  where
    project f pk s = mapMaybe (nonEmptyFoldable . mapMaybe nonZero . Map.fromList . mapMaybe match . Map.toList) $  _store_contracts s
      where
        match (parties, cs) = case fst ps == pk of
          False -> Nothing
          True -> Just (snd ps, cs)
          where
            ps = f parties

delete :: PublicKey -> StateT Store (Either AppError) ()
delete owner = do
  s <- get
  case getIntent owner s of
    Nothing -> throwError $ AppError_Intent VerificationError_NoneInstalled
    Just _ -> pure ()

  modify $ store_intents %~ Map.delete owner

updateClock :: Monad m => Timestamp -> StateT Store m ()
updateClock ts = modify $ store_timestamp .~ ts

setIntent :: Monad m => PublicKey -> Intent -> StateT Store m ()
setIntent owner intent = modify $ store_intents %~ Map.insert owner intent

orIntent :: Monad m => PublicKey -> Intent -> StateT Store m ()
orIntent owner intent = modify $ store_intents %~ f
  where
    f intents =
      let
        old = Map.lookup owner intents
        new = maybe intent (Intents.or intent) old
      in
        Map.insert owner new intents

burn :: Monad m => PublicKey -> Token -> Amount -> StateT Store m ()
burn onAddress token amount = modify $ modifyBalance token onAddress (\x -> x - amount)

mint :: Monad m => PublicKey -> Token -> Amount -> StateT Store m ()
mint onAddress token amount = modify $ modifyBalance token onAddress (+ amount)

transfer :: Monad m => (PublicKey, PublicKey) -> Asset (,) -> StateT Store m ()
transfer (from, to) (token, amount) = do
  burn from token amount
  mint to token amount

--TODO: check for underflow
rights :: Monad m => (PublicKey, PublicKey) -> (Intent, PublicKey) -> StateT Store m ()
rights (seller, buyer) (i, issuer) = do
  modify $ f (issuer, buyer)  succ
  modify $ f (issuer, seller) pred
  where
    f k inner = store_contracts %~ outer i
      where
        outer = Map.alter $ nonEmptyFoldable . middle k . fromMaybe mempty
        middle = Map.alter $ nonZero . inner . fromMaybe 0

issue :: Monad m => PublicKey -> PublicKey -> Intent -> StateT Store m ()
issue issuer holder i = modify $ store_contracts %~ Map.alter (nonEmptyFoldable . f . fromMaybe mempty) i
  where
    f = Map.unionWith (+) one
    one = Map.singleton (issuer, holder) 1
