{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Saline.Network.Node.Transaction where

import Prelude hiding (filter)

import Control.Applicative
import Control.Error (note)
import Control.Lens (Wrapped (..), ifor, imap, iso, ix, lazy, makeLenses, over, strict, view, (.~), (^?), _Unwrapped')
import Control.Monad (guard, join)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON, ToJSON, (.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.GADT.TH (deriveJSONGADT)
import Data.Aeson.Types qualified as Aeson
import Data.Bifunctor
import Data.Bool
import Data.ByteArray.Base64String (Base64String (..))
import Data.ByteArray.HexString (HexString (..))
import Data.ByteArray.HexString qualified as HexString
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.Char qualified as Char
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Dependent.Map qualified as DMap
import Data.Dependent.Sum
import Data.Either (partitionEithers)
import Data.ExtendedReal (Extended (NegInf, PosInf, Finite))
import Data.Foldable
import Data.Function
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Interval (Interval, (<..<=), (<=..<), (<..<))
import Data.Interval qualified as Interval
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Traversable
import Data.Tuple (swap)
import Data.Universe
import Data.Universe.Some
import Data.Universe.Some.TH
import Data.UUID (UUID)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Obelisk.Route (pattern (:.), (:.), encode, utf8Encoder)
import Witherable

import Saline.Crypto
import Saline.Intents (Contracts, Intent, Balances)
import Saline.Intents.Condition
import Saline.Intents.Witness
import Saline.Utils
import Saline.Utils.TH
import Saline.Integrations.Aeson
import Saline.Integrations.Bindings

type Nonce = UUID
type Height = Word32

-- TODO: remove
type TrustedWatcher = PublicKey

-- TODO: trustless
type LockProof = TrustedWatcher

data BridgeInstructionTag a where
  BridgeInstructionTag_Burn :: BridgeInstructionTag (Token, Amount)
  BridgeInstructionTag_Mint :: BridgeInstructionTag (LockProof, Token, Amount)

type BridgeInstruction = DSum BridgeInstructionTag Identity

{-# COMPLETE BridgeInstruction_Burn, BridgeInstruction_Mint #-}
pattern BridgeInstruction_Burn :: Token -> Amount -> BridgeInstruction
pattern BridgeInstruction_Burn t a = BridgeInstructionTag_Burn :=> Identity (t,a)

pattern BridgeInstruction_Mint :: LockProof -> Token -> Amount -> BridgeInstruction
pattern BridgeInstruction_Mint p t a = BridgeInstructionTag_Mint :=> Identity (p, t,a)

data InstructionTag a where
  InstructionTag_Funds :: InstructionTag (PublicKey, PublicKey, (Asset Map))
  InstructionTag_OrIntent :: InstructionTag (PublicKey, Intent)
  InstructionTag_SetIntent :: InstructionTag (PublicKey, Intent)
  InstructionTag_Delete :: InstructionTag PublicKey
  InstructionTag_Bridge :: InstructionTag (PublicKey {- account being bridged -}, BridgeInstruction)

type Instruction = DSum InstructionTag Identity

{-# COMPLETE Instruction_Funds, Instruction_OrIntent, Instruction_SetIntent, Instruction_Delete, Instruction_Bridge#-}

pattern Instruction_Funds :: PublicKey -> PublicKey -> Asset Map -> Instruction
pattern Instruction_Funds from to assets = InstructionTag_Funds :=> Identity (from, to, assets)

pattern Instruction_OrIntent :: PublicKey -> Intent -> Instruction
pattern Instruction_OrIntent host i = InstructionTag_OrIntent :=> Identity (host, i)

pattern Instruction_SetIntent :: PublicKey -> Intent -> Instruction
pattern Instruction_SetIntent host i = InstructionTag_SetIntent :=> Identity (host, i)

pattern Instruction_Delete :: PublicKey -> Instruction
pattern Instruction_Delete host = InstructionTag_Delete :=> Identity host

pattern Instruction_Bridge :: PublicKey -> BridgeInstruction -> Instruction
pattern Instruction_Bridge pk bi = InstructionTag_Bridge :=> Identity (pk, bi)

deriveMany [id]
  [ deriveArgDict      ''BridgeInstructionTag
  , deriveStockGADT    ''BridgeInstructionTag
  , deriveJSONGADT     ''BridgeInstructionTag
  , deriveUniverseSome ''BridgeInstructionTag
  , deriveArgDict      ''InstructionTag
  , deriveStockGADT    ''InstructionTag
  , deriveJSONGADT     ''InstructionTag
  , deriveUniverseSome ''InstructionTag
  ]
instance FiniteSome BridgeInstructionTag
instance FiniteSome InstructionTag

type IntentsMask = Map PublicKey (Either (PublicKey, Intent) Intent) --TODO: more granularity?

data Tx = Tx
  { _tx_instructions :: NonEmpty Instruction
  , _tx_witnesses :: Map PublicKey Witness
  , _tx_arguments :: VariableValues
  , _tx_intentMask :: IntentsMask
  }
  deriving stock (Eq, Ord, Show, Generic)

makeLenses ''Tx

type SignedTx = Signed Tx
data Signed a = Signed
  { _signed_nonce :: Nonce
  , _signed_signature :: Signature
  , _signed_signee :: a
  , _signed_signers :: NonEmpty PublicKey --TODO: require uniqueness?
  }
  deriving stock (Eq, Ord, Read, Show, Generic)

data Hash (a :: Type) where
  Hash_Tx :: ByteString -> Hash SignedTx -- 32 bytes. TODO: is it worth enforcing length at type level?

deriving stock instance Eq  (Hash a)
deriving stock instance Ord (Hash a)

instance Show (Hash SignedTx) where
  show (Hash_Tx bs) = Text.unpack $ HexString.format $ HexString bs


--TODO: Word vs Int (negate 1)
data Flows f = Flows
  { _flows_funds     :: Map Token (f Amount)
  , _flows_rights    :: Map (Intent, PublicKey) (f Int)
  , _flows_issuances :: Map (Intent, Role) (f Word)
  }
deriving stock instance (Eq  (f Amount), Eq  (f Int), Eq  (f Word)) => Eq  (Flows f)
deriving stock instance (Ord (f Amount), Ord (f Int), Ord (f Word)) => Ord (Flows f)

noFlows :: Flows f
noFlows = Flows mempty mempty mempty

mergeFlows :: forall f. (forall k v. (Ord k, Ord v, Num v) => Map k (f v) -> Map k (f v) -> Map k (f v)) -> Flows f -> Flows f -> Flows f
mergeFlows op a b = Flows
  { _flows_funds     = g _flows_funds
  , _flows_rights    = g _flows_rights
  , _flows_issuances = g _flows_issuances
  }
  where
    g :: forall k v. (Ord k, Ord v, Num v) => (Flows f -> Map k (f v)) -> Map k (f v)
    g fld = (op `on` fld) a b

intersectFlows :: Flows Interval -> Flows Interval -> Flows Interval
intersectFlows = mergeFlows $ Map.unionWith Interval.intersection

traverseFlows :: forall f g h. Applicative h => (forall k v. (Ord k, Real v) => Map k (f v) -> h (Map k (g v))) -> Flows f -> h (Flows g)
traverseFlows f a = pure Flows
  <*> g _flows_funds
  <*> g _flows_rights
  <*> g _flows_issuances
  where
    g :: forall k v. (Ord k, Real v) => (Flows f -> Map k (f v)) -> h (Map k (g v))
    g fld = f $ fld a

trySampleFlows :: Flows Interval -> Maybe (Flows Identity)
trySampleFlows = traverseFlows $ traverse $ fmap Identity . sample . Interval.lowerBound
  where
    sample = \case
      Finite r -> Just r
      NegInf -> Nothing
      PosInf -> Nothing

makeLenses ''Flows

onlyFunds :: Map Token (f Amount) -> Flows f
onlyFunds x = noFlows & flows_funds .~ x

onlyIssuances :: Map (Intent, Role) (f Word) -> Flows f
onlyIssuances x = noFlows & flows_issuances .~ x

onlyRights :: Map (Intent, PublicKey) (f Int) -> Flows f
onlyRights x = noFlows & flows_rights .~ x

-- TODO: should the solver be responsible for applying this or always the UI?
disregardingInstalled :: PublicKey -> Tx -> Tx
disregardingInstalled pk = over tx_intentMask $ Map.insert pk $ Right $ Condition_Signature pk

optsSignedTx :: Aeson.Options
optsSignedTx = Aeson.defaultOptions { Aeson.fieldLabelModifier = noPrefix "_signed_" }
{-
--TODO: watcher-specific overrides - remove?
instance ToJSON SignedTx where
  toJSON = Aeson.genericToJSON optsSignedTx

instance FromJSON SignedTx where
  parseJSON v = Aeson.genericParseJSON optsSignedTx v <|> parse v
    where
      parse = Aeson.withObject "SignedTx" $ \o -> do
        signature <- o .: "signature"
        signers <- o .: "signers"
        (nonce, signee) <- parseNonceTx =<< o .: "tx"
        pure $ Signed signature signers signee nonce

      parseNonceTx = Aeson.withObject "Tx" $ \o -> do
        nonce <- o .: "nonce"
        "mint" :: Text <- o .: "type"
        account <- o .: "account"
        token <- o .: "token"
        amount <- o .: "amount"
        proof <- o .: "proof"
        let
          -- TODO: apply intents to bridges
          tx = simpleTx $ pure $ Instruction_Bridge account $ BridgeInstruction_Mint proof token amount
        pure (nonce, tx)
-}
data TxError
  = TxError_Decode Text
  | TxError_Parse Text
  | TxError_Signature Text
  | TxError_Execution AppError
  deriving stock (Eq, Show)

data AppError
  = AppError_InsufficientFunds PublicKey Token Amount
  | AppError_Intent VerificationError
  | AppError_Unsigned PublicKey
  | AppError_Untrusted -- TODO: remove trust-based requirements alltogether
  deriving stock (Eq, Ord, Show)

data VerificationError
  = VerificationError_Disallowed (Either WitnessError (Map PublicKey Intent))
  | VerificationError_Locked
  | VerificationError_NoneInstalled
  deriving stock (Eq, Ord, Show)

data WitnessError = WitnessError
  deriving stock (Eq, Ord, Show)

prettyPrintTxError :: TxError -> Text
prettyPrintTxError = \case
  TxError_Decode e -> "Failed decode: " <> e
  TxError_Parse e -> "Failed parse: " <> e
  TxError_Signature e -> "Invalid signature: " <> e
  TxError_Execution ae -> case ae of
    AppError_InsufficientFunds pk t a -> Text.unwords ["Would need another", tshow (negate a), tshow t, "on wallet", renderAddress pk]
    AppError_Intent ie -> case ie of
      VerificationError_Disallowed err -> either (const "Invalid transaction") renderRejections err
      VerificationError_NoneInstalled -> "No intent is installed"
      VerificationError_Locked -> "Cannot modify intent. It has a time lock"
    AppError_Unsigned pk -> "Transaction requires signature: " <> renderAddress pk
    AppError_Untrusted -> "Unauthorized"
  where
    renderToken = tshow
    renderTimestamp = timestamp . realToFrac
    renderAddress = addressEllipsis
    renderVar (Var _ name) = "$" <> name
    renderAmount = \case
      Expr_Lit a -> tshow a
      Expr_Balance t -> tshow t <> " balance"
      Expr_Receiving t -> "Incoming " <> renderToken t
      Expr_Sending t -> "Outgoing " <> renderToken t
      Expr_Var v -> renderVar v
      Expr_Arithmetic2 (Binary lhs op rhs) -> Text.unwords [renderAmount lhs, symbol op, renderAmount rhs]

    nest h = (h:) . fmap ("  " <>)

    renderRejections rs = Text.unlines $ nest "Rejected by" $ fold $ toList $ imap renderRejection rs
    renderRejection pk r = nest (renderAddress pk <> " requires:") $ renderIntent r
    renderIntent = \case
      Condition_Counterparty s -> pure $ "Counterparty is " <> renderAddress s
      Condition_Signature s -> pure $ "Signature from " <> renderAddress s
      Condition_Restriction (Binary lhs rel rhs) -> pure $ Text.unwords
        [ "Constraint not met:"
        , renderAmount lhs
        , ppRelation rel
        , renderAmount rhs
        ]
      Condition_All cs -> nest "All of" $ renderIntent =<< cs
      Condition_Any n cs -> nest ("At least " <> tshow n <> " among") $ renderIntent =<< cs
      Condition_Finite _ c -> renderIntent c
      Condition_Temporary ts availableAfter c -> case availableAfter of
        False -> renderIntent c
        True -> pure $ "Not available until " <> renderTimestamp ts

parseTx :: Base64String -> Either TxError SignedTx
parseTx = parseTx' pure

parseVerifyTx :: Base64String -> Either TxError SignedTx
parseVerifyTx = parseTx' $ \a -> do
  stx <- first (TxError_Parse . Text.pack) $ Aeson.parseEither p a
  let s = serializeTx (_signed_nonce stx, _signed_signee stx) --TODO: avoid re-serialization?
  for_ (aggregateVerify (_signed_signers stx) s (_signed_signature stx)) $
    throwError . TxError_Signature
  pure stx
  where
    p = Aeson.withObject "SignedTx" $ \o -> do
      signature <- o .: "signature"
      signers   <- o .: "signers"
      signee    <- o .: "signee"
      nonce     <- o .: "nonce"
      pure $ Signed
        { _signed_nonce  = nonce
        , _signed_signature = signature
        , _signed_signee = signee
        , _signed_signers  = signers
        }


parseTx' :: (FromJSON a, MonadError TxError m) => (a -> m b) -> Base64String -> m b
parseTx' f (Base64String bs) = case parseAeson =<< parseBase16 bs of
  Left err -> throwError $ TxError_Decode err
  Right a -> f a
  where
    parseBase16 = Base16.decodeBase16Untyped
    parseAeson = first Text.pack . Aeson.eitherDecode' . view lazy

renderHash :: Hash a -> Text
renderHash = \case
  Hash_Tx bs -> HexString.format $ HexString bs

parseHashTx :: Text -> Either Text (Hash SignedTx)
parseHashTx txt = do
  hex <- note "No leading '0x'" $ Text.stripPrefix "0x" txt
  note "Wrong size" $ guard $ Text.length hex == 64
  note "Wrong char" $ guard $ Text.all Char.isHexDigit hex
  bimap Text.pack (Hash_Tx . HexString.unHexString) $ HexString.hexString $ encodeUtf8 hex

txErrorToTxData :: TxError -> Base64String
txErrorToTxData = Base64String . encode @(Either Text) utf8Encoder . prettyPrintTxError

prepareTx :: SecretKey -> (PublicKey -> Tx) -> (Tx, SecretKey)
prepareTx sk f = (f (derivePublicKey sk), sk)

mkSignedTx :: MonadIO m => (Tx, SecretKey) -> m SignedTx
mkSignedTx = liftIO . liftA2 mkSignedTxWithNonce genUUID . pure

mkSignedTxWithNonce :: Nonce -> (Tx, SecretKey) -> SignedTx
mkSignedTxWithNonce nonce (tx, sk) = Signed
  { _signed_signature = mkTxSignature sk (nonce, tx)
  , _signed_signers = pure $ derivePublicKey sk
  , _signed_signee = tx
  , _signed_nonce = nonce
  }

addSignature :: SecretKey -> SignedTx -> SignedTx
addSignature sk stx =
  let s = mkTxSignature sk $ (_signed_nonce stx, _signed_signee stx)
  in stx
    { _signed_signature = aggregateSignatures $ _signed_signature stx :| [s]
    , _signed_signers = _signed_signers stx <> pure (derivePublicKey sk)
    }

addSignatures :: Foldable f => f SecretKey -> SignedTx -> SignedTx
addSignatures = flip $ foldl' $ flip addSignature

mkTxSignature :: SecretKey -> (Nonce, Tx) -> Signature
mkTxSignature sk = sign sk . serializeTx

serializeTx :: forall a. ToJSON a => a -> ByteString
serializeTx = view strict . Aeson.encode

contractFlows :: NonEmpty Instruction -> Map (PublicKey, PublicKey) (Contracts Word)
contractFlows instructions = Map.unionsWith (Map.unionWith (+)) $ toList $ flip fmap instructions $ \case
  _ -> mempty

assetFlows :: NonEmpty Instruction -> Map PublicKey (Map Counterparty (Flows Identity))
assetFlows instructions = Map.unionsWith (Map.unionWith (mergeFlows $ Map.unionWith (+))) $ toList $ flip fmap instructions $ \case
  Instruction_Funds from to assets -> case from == to of
    True  -> mempty
    False -> fold
      [ Map.singleton to   $ Map.singleton (Just from) $ onlyFunds $ fmap Identity assets
      , Map.singleton from $ Map.singleton (Just to)   $ onlyFunds $ fmap (Identity . negate) assets
      ]

  Instruction_Bridge bridgedAccount bridgeTx -> case bridgeTx of
    BridgeInstruction_Burn   token amount -> Map.singleton bridgedAccount $ Map.singleton Nothing $ onlyFunds $ Map.singleton token $ Identity $ negate amount
    BridgeInstruction_Mint _ token amount -> Map.singleton bridgedAccount $ Map.singleton Nothing $ onlyFunds $ Map.singleton token $ Identity $ amount

  Instruction_Delete {} -> mempty
  Instruction_OrIntent {} -> mempty
  Instruction_SetIntent {} -> mempty

flowPairs :: NonEmpty Instruction -> Map (Counterparty, Counterparty) (Asset Map, Asset Map)
flowPairs instructions =
  let
    collectFlowsWith f = merge $ fmap f $ toList instructions

    merge = Map.unionsWith $ \(a,b) (a',b') -> (addAssets a a', addAssets b b')

    addAssets = Map.unionWith (+)

    canonicalize a b = case compare a b of
      LT -> Map.singleton (Just a, Just b)
      EQ -> const mempty
      GT -> Map.singleton (Just b, Just a) . swap
  in
    collectFlowsWith $ \case
      Instruction_Funds from to assets ->
        let (forward, backward) = Map.partition (> 0) assets
        in canonicalize from to (forward, fmap negate backward)

      Instruction_Bridge bridgedAccount bridgeTx -> case bridgeTx of
        BridgeInstruction_Burn   token amount -> Map.singleton (Just bridgedAccount, Nothing) (Map.singleton token amount, mempty)
        BridgeInstruction_Mint _ token amount -> Map.singleton (Nothing, Just bridgedAccount) (Map.singleton token amount, mempty)
      _ -> mempty

counterpartyVar :: Text
counterpartyVar = "counterparty"

--TODO: how is use of this affected by addition of new assets?
defaultIntent :: PublicKey -> Intent
defaultIntent pk = Condition_Any 1
  [ Condition_Signature pk
  , Condition_Any 1 $ flip fmap universeF $ \t ->
      Condition_Restriction $ Binary (Expr_Receiving t) Relation_GT (Expr_Lit 0)
  ]

counterpartyVariableValues :: Text -> PublicKey -> VariableValues
counterpartyVariableValues var = DMap.singleton VariableTag_Address . Compose . Map.singleton var . Identity

verifyFlow
  :: Balances
  -> VariableValues
  -> NonEmpty PublicKey
  -> Map Counterparty (Flows Identity)
  -> Witness
  -> Intent
  -> Either WitnessError (Maybe Intent)
verifyFlow balances vals signers flows = go
  where
    go :: Witness -> Intent -> Either WitnessError (Maybe Intent)
    go = curry $ \case
      (w, Condition_Finite _ c) -> go w c

      (w, c@(Condition_Temporary _ availableAfter i)) -> case availableAfter of
        False -> go w i
        True -> Right $ Just c

      (Witness_Auto, c@(Condition_Restriction (Binary lhs rel rhs))) -> Right $ maybe (Just c) (\() -> Nothing) $ do
        holds <- liftA2 (evalRelation rel) (resolveExpr lhs) (resolveExpr rhs)
        guard holds

      --TODO: can we always require witness?
      (Witness_Auto, c@(Condition_Any _ cs)) -> go (Witness_Any subWitnesses) c
        where subWitnesses = Map.fromList $ flip imap cs $ \i _ -> (toEnum i, Witness_Auto)

      (Witness_Any ws, Condition_Any threshold cs) -> do
        branches <- fmap toList $ ifor ws $ \idx w -> case cs ^? ix (fromEnum idx) of
          Nothing -> throwError WitnessError
          Just c -> go w c
        let
          (failed, passed) = partitionEithers $ flip fmap branches $ maybe (Right ()) Left
          progress = toEnum $ length passed
        pure $ case progress >= threshold of
          True -> Nothing
          False -> Just $ Condition_Any (threshold - progress) failed

      (Witness_Any {},   _) -> Left WitnessError

      (Witness_Auto  , Condition_All cs) -> go (Witness_All $ Witness_Auto <$ cs) (Condition_All cs)
      (Witness_All ws, Condition_All cs) -> case length ws == length cs of
        False -> Left WitnessError
        True -> do
          branches <- sequence $ zipWith go ws cs
          pure $ fmap (Condition_All . toList) $ nonEmpty $ catMaybes branches
      (Witness_All {},   _) -> Left WitnessError

      (Witness_Auto, c@(Condition_Counterparty address)) -> Right $ case all (== Just address) (Map.keys flows) of
        False -> Just c
        True -> Nothing

      (Witness_Auto, Condition_Signature signer) -> Right $ case signer `elem` signers of
        False -> Just $ Condition_Signature signer
        True -> Nothing

      where
        resolveExpr = \case
          Expr_Lit a -> Just a
          Expr_Balance t -> Just $ sum $ fmap (fromMaybe 0) [old, change]
            where
              old = Map.lookup t balances
              change = tokenFlow t

          Expr_Var v -> lookupVar v vals --TODO: why require a variable name then?
          Expr_Receiving f -> tokenFlow f
          Expr_Sending f -> fmap negate $ tokenFlow f
          Expr_Arithmetic2 (Binary lhs op rhs) -> liftA2 (evalOp op) (resolveExpr lhs) (resolveExpr rhs)

        tokenFlow t = Just $ sum $ fmap funds flows
          where funds = maybe 0 runIdentity . Map.lookup t . _flows_funds

type Solution = (Tx, Set PublicKey)
type Requirements = Witness :. VariableValues :. Map (PublicKey, PublicKey) (Flows Interval) :. Set PublicKey

-- TODO: match on both rather than rely on submitter signature as intent/witness?
trySolve :: PublicKey -> PublicKey -> Contracts (NonEmpty PublicKey) -> Intent -> [Solution]
trySolve fulfiller host _ = \case
  Condition_Any 1 cs -> mapMaybe mkSolvedTx $ solveAny cs
  c -> toList $ mkSolvedTx $ go c
  where
    go :: Intent -> Maybe Requirements
    go = \case
      Condition_Any 1 cs -> case catMaybes $ solveAny cs of
        [s] -> Just s
        _ -> Nothing

      Condition_Any _ _  -> Nothing

      --TODO: what about collisions? fail? take max?
      Condition_All cs -> fmap aggregate $ traverse go cs

      Condition_Finite _ c -> go c

      Condition_Temporary _ _ c -> go c

      Condition_Restriction r -> asum $ fmap solve [r, flipRestriction r]
        where
          solve (Binary lhs rel rhs) = do
            l <- case lhs of
              Expr_Lit a -> Just a
              _ -> Nothing

            (t, mkFlow) <- case rhs of
              Expr_Sending t -> pure (t, Map.singleton . swap)
              Expr_Receiving t -> pure (t, Map.singleton)
              _ -> Nothing

            let
              i = case rel of
                Relation_EQ -> Interval.singleton l
                Relation_LT -> Finite l <..< PosInf
                Relation_LE -> Finite l <=..< PosInf
                Relation_GT -> NegInf <..< Finite l
                Relation_GE -> NegInf <..<= Finite l

            let flow = mkFlow (fulfiller, host) $ onlyFunds $ Map.singleton t i
            Just $ Witness_Auto :. mempty :. flow :. mempty

      Condition_Counterparty _ -> Nothing --TODO: interact with solveParty instead
      Condition_Signature signer -> Just $ Witness_Auto :. mempty :. mempty :. Set.singleton signer

    _solveParty = \case
      Expr_Lit p -> Just p
      Expr_Var (Var _ _) -> Just fulfiller
      _ -> Nothing --TODO: make impossible at compile time

    solveAny cs =
      flip imap cs $ \i c ->
        flip fmap (go c) $
          first $ Witness_Any . Map.singleton (toEnum i)

    aggregate :: [Requirements] -> Requirements
    aggregate xs = Witness_All ws :. fold vs :. Map.unionsWith intersectFlows as :. fold pks
      where
        ws :. vs :. as :. pks = unzip4 xs
        unzip4 = unzips $ unzips unzip
        unzips f = second f . unzip

    mkSolvedTx :: Maybe Requirements -> Maybe Solution
    mkSolvedTx solution = do

      w :. vals :. flowIntervals :. pks <- solution
      flows <- for flowIntervals trySampleFlows
      neflows <- nonEmpty $ Map.toList flows
      instructions <- for neflows $ \((from, to), fs) ->
        let
          funds = Instruction_Funds from to . fmap runIdentity
        in
          nonEmpty $ fold
            [ let ffs = _flows_funds fs in bool [funds ffs] mempty (null ffs)
            ]

      pure $ (,pks) $ Tx
        { _tx_arguments = vals
        , _tx_instructions = join instructions
        , _tx_witnesses = Map.singleton host w
        , _tx_intentMask = mempty
        }

lookupVar :: Var a -> VariableValues -> Maybe a
lookupVar (Var tag name) vals = do
  Compose m <- DMap.lookup tag vals
  Identity lit <- Map.lookup name m
  pure lit

simpleTx :: NonEmpty Instruction -> Tx
simpleTx is = Tx
  { _tx_arguments    = mempty
  , _tx_instructions = is
  , _tx_witnesses    = mempty
  , _tx_intentMask   = mempty
  }

intentSizeHeuristic :: Intent -> Word
intentSizeHeuristic = fix $ \go -> \case
  Condition_All cs -> 1 + sum (fmap go cs)
  Condition_Any _ cs -> 1 + sum (fmap go cs)
  Condition_Counterparty {} -> 1
  Condition_Finite _ c -> 1 + go c
  Condition_Temporary _ _ c -> 1 + go c
  Condition_Restriction b -> 1 + sum (fmap exprSizeHeuristic b)
  Condition_Signature {} -> 1

exprSizeHeuristic :: Expr a -> Word
exprSizeHeuristic = fix $ \go -> newtypeDSum $ \case
  ExprTag_Lit -> const 1
  ExprTag_Balance -> const 1
  ExprTag_Var -> const 1
  ExprTag_Arithmetic2 -> \(Identity b) -> 1 + sum (fmap go b)
  ExprTag_Receive -> const 1
  ExprTag_Send -> const 1

-- Optics

instance Wrapped (Signed a) where
  type Unwrapped (Signed a) = (Nonce, Signature, a, NonEmpty PublicKey)
  _Wrapped' = iso u w
    where
    u (Signed a b c d) = (a,b,c,d)
    w (a,b,c,d) = (Signed a b c d)

-- Serialisation
txType :: Binding Tx
txType = Binding_Defined "Transaction"

signedTxType :: Binding SignedTx
signedTxType = Binding_Defined "Signed"

instructionType :: Binding Instruction
instructionType = Binding_Defined "Instruction"

bridgeInstructionType :: Binding BridgeInstruction
bridgeInstructionType = Binding_Defined "BridgeInstruction"


signedTx_ :: Named (Struct SignedTx)
signedTx_ = named "Signed" $ isomap _Unwrapped' $ tuple4
  (field "nonce"     $ uuidType)
  (field "signature" $ signatureType)
  (field "signee"    $ txType)
  (field "signers"   $ list1Type publicKeyType)

tx_ :: Named (Struct Tx)
tx_ = named "Transaction" $ isomap (iso fw bw)
  (field "instructions" $ Binding_NEList instructionType)
  where
    fw a = Tx a mempty mempty mempty --TODO: unsafe
    bw (Tx a _ _ _) = a --TODO: unsafe

instruction_ :: Named (Union InstructionTag)
instruction_ = named "Instruction" $ union $ \case
  InstructionTag_Funds -> named "TransferFunds" $ triple
    (field "source" publicKeyType)
    (field "target" publicKeyType)
    (field "funds" $ dictType tokenType float64Type)
  InstructionTag_SetIntent -> named "SetIntent" $ pair
    (field "host" publicKeyType)
    (field "intent" intentType)
  InstructionTag_OrIntent -> named "OrIntent" $ pair
    (field "host" publicKeyType)
    (field "intent" intentType)
  InstructionTag_Delete -> named "Delete" $
    field "host" publicKeyType
  InstructionTag_Bridge -> named "Bridge" $ pair
    (field "bridgedAccount" publicKeyType)
    (field "instruction" bridgeInstructionType)

bridgeInstruction_ :: Named (Union BridgeInstructionTag)
bridgeInstruction_ = named "BridgeInstruction" $ union $ \case
  BridgeInstructionTag_Burn -> named "Burn" $ pair
    (field "token" tokenType)
    (field "amount" float64Type)
  BridgeInstructionTag_Mint -> named "Mint" $ triple
    (field "prover" publicKeyType)
    (field "token" tokenType)
    (field "amount" float64Type)

instance FromJSON BridgeInstruction where parseJSON = parseJSONUnion bridgeInstruction_
instance ToJSON   BridgeInstruction where toJSON = toJSONUnion bridgeInstruction_

instance FromJSON Instruction where parseJSON = parseJSONUnion instruction_
instance ToJSON   Instruction where toJSON = toJSONUnion instruction_

instance FromJSON Tx where parseJSON = parseJSONStruct tx_
instance ToJSON   Tx where toJSON = toJSONStruct tx_

instance FromJSON SignedTx where parseJSON = parseJSONStruct signedTx_
instance ToJSON   SignedTx where toJSON = toJSONStruct signedTx_
