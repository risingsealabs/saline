{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE NoMonomorphismRestriction #-}
module Saline.App.Editor.Intents.Editor where

import Clay qualified
import Control.Applicative
import Control.Arrow ((&&&))
import Control.Lens qualified as Lens
import Control.Monad ((<=<))
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader (ask, local), ReaderT (runReaderT), asks)
import Data.Bifunctor
import Data.Dependent.Map qualified as DMap
import Data.Dependent.Sum
import Data.Foldable
import Data.Functor.Compose
import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Set qualified as Set
import Data.Some
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time
import Data.Universe
import Data.Universe.Some
import Reflex.Dom.Core hiding (Alt, list, tag)
import Text.Printf

import Saline.App.Editor.Field
import Saline.App.Utils qualified as Utils
import Saline.App.Editor.Utils.Editor qualified as Editor
import Saline.App.Editor.Utils.Reflex
import Saline.App.Editor.Widgets
import Saline.App.Layout
import Saline.App.Widgets (formDropdown)
import Saline.Crypto (parseAddress, prettyPrintAddress)
import Saline.Intents
import Saline.Intents.Analysis () -- currently unused, just imported so ghcid picks it up
import Saline.Intents.Condition
import Saline.Utils
import Saline.Utils.Field

type Editor t m a = Editor.Editor t m Text a a

type HasEditorEnv t m = MonadReader (Hold t NominalDiffTime, Dynamic t VariableNames) m

instrument
  :: (DomBuilder t m, MonadFix m, MonadHold t m, MonadIO m, MonadIO (Performable m), PerformEvent t m, PostBuild t m, TriggerEvent t m)
  => Editor t m (VariableNames, Intent)
instrument i0 = column blank $ do
  let
    fromParameters d = do
      tag :=> v <- DMap.toList d
      name <- toList $ getConst v
      pure (name, Some tag)

    --TODO: name collisions?
    toParameters = DMap.fromListWithKey (const (<>)) . fmap f
      where f (name, Some tag) = tag :=> Const (Set.singleton name)

  pars <- row (Clay.justifyContent Clay.center) $
    column (Clay.justifyContent Clay.center *> Clay.textAlign Clay.center) $ do
      elAttr "div" (style $ moatCss $ Just Clay.transparent) $ do
        (fmap . fmap . fmap) toParameters $
          expanding parameter $ maybe [] (fromParameters . fst) i0

  pars' <- (fmap . fmap) (fromMaybe mempty . justFieldValue) $ withHoldM pars holdDyn

  tick0 <- liftIO getPOSIXTime
  tick <- tickLossyFromPostBuildTime 1
  tickEv <- performEvent $ liftIO getPOSIXTime <$ tick
  let tickHold = mkHold tick0 tickEv

  let
    col autoFocus c = flip runReaderT (tickHold, pars') $ do
      condition autoFocus list' c

  row (Clay.justifyContent Clay.spaceEvenly) $ do
    i  <- col False $ fmap snd i0
    pure $ getCompose $ pure (,)
      <*> Compose pars
      <*> Compose i

identifier :: (DomBuilder t m, MonadFix m, MonadHold t m) => Editor t m Identifier
identifier = nonEmptyInput pure . fromMaybe ""

parameter
  :: (DomBuilder t m, MonadFix m, MonadHold t m)
  => Editor t m (Text, Some VariableTag)
parameter x0 = row blank $ do
  let
    none = (Nothing, "-- Parameter --")
    opts = Map.fromList $ (none:) $ ffor (sort universeF) $ \t -> (Just t,) $ withSome t $ \case
      VariableTag_Address -> "Address"
      VariableTag_Amount  -> "Amount"
      VariableTag_Count   -> "Count"
      VariableTag_List    -> "List"

  par <- formDropdown (fmap snd x0) opts id
  name <- indented $ identifier $ fmap fst x0
  pure $ getCompose $ pure (,) <*> Compose name <*> Compose (fmap maybeField par)

timing :: (DomBuilder t m, MonadHold t m, MonadFix m, HasEditorEnv t m) => Editor t m (DiffTime, Bool)
timing = Editor.pair duration locked $ \d l -> do
  l' <- labeled "Available" l
  d' <- labeled "Seconds" d
  timeDyn <- asks fst
  let d'' = ffor2 timeDyn d' $ \t -> fmap (realToFrac t +)
  pure (d'', l')

relation :: DomBuilder t m => Editor t m Relation
relation rel0 = do
  let
    none = (Nothing, "-- Relation --")
    opts = Map.fromList $ (none:) $ ffor (sort universeF) $ Just &&& ppRelation
  (fmap . fmap) maybeField $ formDropdown rel0 opts id

restriction
  :: DomBuilder t m
  => Editor t m a
  -> Editor t m (Binary Relation a)
restriction object = Editor.iso Lens._Unwrapped' $ Editor.triple object relation object $ \lhs rel rhs -> do
  let
    c = divClass "flex flex-col gap-2 items-start"
    r = divClass "flex flex-row gap-2"
  c $ liftA3 (,,)
    (r lhs)
    rel
    (r rhs)

condition
  :: forall t m l
   . (DomBuilder t m, MonadFix m, MonadHold t m, HasEditorEnv t m, PostBuild t m)
  => Bool
  -> (Editor t m (Condition Expr Expr l) -> Editor t m (l (Condition Expr Expr l)))
  -> Editor t m (Condition Expr Expr l)
condition _autoFocus mkList = conditionField $ column (Clay.alignItems Clay.flexStart) . dsumEditor "Condition" universeF showOption subEditor
  where
    policies' = mkList (condition False mkList)

    showOption :: Some (ConditionTag Expr Expr l) -> Text
    showOption = foldSome $ \case
      ConditionTag_All          -> "All"
      ConditionTag_Any          -> "Any"
      ConditionTag_Counterparty -> "Counterparty"
      ConditionTag_Finite       -> "Finite"
      ConditionTag_Temporary    -> "Temporary"
      ConditionTag_Restriction  -> "Restriction"
      ConditionTag_Signature    -> "Signature"

    subEditor :: ConditionTag Expr Expr l x -> Editor t m x
    subEditor = \case
      ConditionTag_All -> nested . column (Clay.alignItems Clay.flexStart) . policies'
      ConditionTag_Any -> Editor.pair num policies' $ \t p ->
        indented $ column (Clay.alignItems Clay.flexStart) $ liftA2 (,) (labeled "Threshold" t) $ do
          el "label" $ text "Elements"
          nested p

      ConditionTag_Counterparty -> nested . party
      ConditionTag_Finite -> finite
      ConditionTag_Temporary -> temporary
      ConditionTag_Restriction -> nested . restriction (expr VariableTag_Amount amount)
      ConditionTag_Signature -> nested . signer

      where
        finite :: Editor t m (Uses, Condition Expr Expr l)
        finite = Editor.pair num (condition False mkList) $ \t -> liftA2 (,) (labeled "Uses" t)

        temporary :: Editor t m (DiffTime, Bool, Condition Expr Expr l)
        temporary = Editor.iso (Lens.iso fw bw) $ Editor.pair timing (condition False mkList) $ \t -> liftA2 (,) (labeled "Timing" t)
          where
            fw ((d,a), c) = (d, a, c)
            bw (d, a, c) = ((d,a), c)

transfer
  :: (DomBuilder t m, MonadFix m, MonadHold t m, HasEditorEnv t m, PostBuild t m)
  => (Text, Editor t m Role)
  -> (Maybe Text, Editor t m a)
  -> Editor t m (Transfer a Expr)
transfer (roleL, role) (assetsL, assets) = Editor.iso Lens._Unwrapped' $ Editor.triple (expr VariableTag_Address party) role assets $ \counterparty role' assets' ->
  column (Clay.alignItems Clay.flexStart) $ do
    d  <- labeled roleL role'
    p  <- labeled "Counterparty" counterparty
    as <- maybe id labeled assetsL assets'
    pure (p, d, as)

transferFunds
  :: (DomBuilder t m, MonadFix m, MonadHold t m, HasEditorEnv t m, PostBuild t m)
  => Editor t m (Transfer (AssetF Expr Map) Expr)
transferFunds = transfer ("Direction", direction) (Just "Funds", funds)

transferRights
  :: (DomBuilder t m, MonadFix m, MonadHold t m, HasEditorEnv t m, PostBuild t m)
  => Editor t m (Condition Expr Expr l)
  -> Editor t m (Transfer (Condition Expr Expr l, Expr Address) Expr)
transferRights go = transfer ("Direction", direction) (Nothing, rights go $ expr VariableTag_Address party)

rights
  :: DomBuilder t m
  => Editor t m (Condition e p l)
  -> Editor t m (p Address)
  -> Editor t m (Rights e p l)
rights go p = Editor.pair go p $ \c i -> do
  i' <- labeled "Issuer" i
  c' <- labeled "Terms" c
  pure (c', i')

issuance
  :: (DomBuilder t m, MonadFix m, MonadHold t m, HasEditorEnv t m, PostBuild t m)
  => Editor t m (Condition Expr Expr l)
  -> Editor t m (Issuance Expr Expr l)
issuance go = transfer ("Role", hostRole ("Issuer", "Holder")) (Just "Terms", go)

locked :: DomBuilder t m => Editor t m Bool
locked d0 = do
  let
    none = (Nothing, "-- Locked --")
    opts = Map.fromList $ none:
      [ (Just False, "Before")
      , (Just True , "After" )
      ]
  (fmap . fmap) maybeField $ formDropdown d0 opts id

direction :: DomBuilder t m => Editor t m Role
direction = hostRole ("Sending", "Receiving")

boolDropdown :: DomBuilder t m => Text -> (Bool -> Text) -> Editor t m Bool
boolDropdown title entry d0 = do
  let
    none = (Nothing, wrappedSym "--" $ wrappedSym " " title)
    opts = Map.fromList $ none:
      [ (Just True , entry True)
      , (Just False, entry False)
      ]
  (fmap . fmap) maybeField $ formDropdown d0 opts id

hostRole :: DomBuilder t m => (Text, Text) -> Editor t m Role
hostRole (source, sink) r0 = do
  let
    none = (Nothing, wrappedSym "--" $ wrappedSym " " "Role")
    opts = Map.fromList $ none:
      [ (Just Role_Sink, sink)
      , (Just Role_Source, source)
      ]
  (fmap . fmap) maybeField $ formDropdown r0 opts id

flag :: DomBuilder t m => Editor t m Bool
flag val0 = do
  let state0 = fromMaybe False val0
  ev <- Utils.checkbox state0 id
  pure $ mkHold (pure state0) $ ffor ev pure

--TODO: (+) ?
funds
 :: (DomBuilder t m, MonadFix m, MonadHold t m,  HasEditorEnv t m, PostBuild t m)
  => Editor t m (AssetF Expr Map)
funds t0 = (fmap . fmap . fmap) (Map.fromListWith const) $ expanding asset $ maybe [] Map.toList t0

asset
  :: (DomBuilder t m, MonadFix m, MonadHold t m,  HasEditorEnv t m, PostBuild t m)
  => Editor t m (AssetF Expr (,))
asset = Editor.pair token (expr VariableTag_Amount amount) $ \mt me ->
  column (Clay.alignItems Clay.flexStart) $ liftA2 (,) mt $
    nested $ column (Clay.alignItems Clay.flexStart) me

signer, party
  :: (DomBuilder t m, MonadFix m, MonadHold t m)
  => Editor t m Address
signer = nonEmptyInput (eitherField . parseAddress) . maybe "" prettyPrintAddress
party = signer

amount :: (DomBuilder t m, MonadFix m, MonadHold t m) => Editor t m Amount
amount = nonEmptyInput (eitherField . enforcePositive . treadEither) . maybe "" (Text.pack . printf "%f")
  where
    enforcePositive ea = ea >>= \case
      a -> case a > 0 of
        False -> Left "Must be positive"
        True -> pure a

token :: DomBuilder t m => Editor t m Token
token t0 = do
  let
    none = (Nothing, "-- Token --")
    opts = Map.fromList $ (none:) $ ffor (sort universeF) $ Just &&& tshow
  (fmap . fmap) maybeField $ formDropdown t0 opts id

operator2 :: DomBuilder t m => Editor t m Arithmetic2
operator2 op0 = do
  let
    none = (Nothing, "-- Operation --")
    opts = Map.fromList $ (none:) $ ffor (sort universeF) $ Just &&& symbol
  (fmap . fmap) maybeField $ formDropdown op0 opts id

num :: (DomBuilder t m, MonadFix m, MonadHold t m, Read a, Show a) => Editor t m a
num = nonEmptyInput treadField . maybe "" tshow

duration :: (DomBuilder t m, MonadFix m, MonadHold t m) => Editor t m DiffTime
duration = nonEmptyInput (treadField . (<> "s")) . fromMaybe "" . ((Text.stripSuffix "s" . tshow) =<<)

foreach
  :: (DomBuilder t m, MonadFix m, MonadHold t m, HasEditorEnv t m, PostBuild t m)
  => Editor t m (Identifier, Var [Address], Condition Expr Expr List)
foreach = Editor.triple identifier addresses (condition False list) $ \mi mv mc -> do
  i <- labeled "Each"  mi
  v <- labeled "In"    mv
  c <- labeled "Yield" $ letVar VariableTag_Address i mc
  pure (i,v,c)
  where
    addresses = varWithFixedTag VariableTag_List

list
  :: forall t m a.
  ( DomBuilder t m, MonadFix m, MonadHold t m, HasEditorEnv t m, PostBuild t m
  , a ~ Condition Expr Expr List --TODO: generalise
  )
  => Editor t m a
  -> Editor t m (List a)
list valField = Editor.newtypeDSum $ dsumEditor "List" universeF showOption subEditor
  where
    showOption :: Some (ListTag a) -> Text
    showOption = foldSome $ \case
      ListTag_Lit -> "Literal"
      ListTag_For -> "For"

    subEditor :: ListTag a x -> Editor t m x
    subEditor = \case
      ListTag_Lit -> labeled "Elements" . list' valField
      ListTag_For -> foreach

list'
  :: (DomBuilder t m, MonadFix m, MonadHold t m)
  => Editor t m a
  -> Editor t m [a]
list' valField = expanding valField . fromMaybe []

arithmetic2
  :: ( DomBuilder t m, MonadFix m, MonadHold t m, HasEditorEnv t m, PostBuild t m, UniverseSome (ExprTag a)
     )
  => VariableTag a
  -> Editor t m a
  -> Editor t m (Binary Arithmetic2 (Expr a))
arithmetic2 tag valField = Editor.iso Lens._Unwrapped' $ Editor.triple (expr tag valField) operator2 (expr tag valField) $ \mx mop my -> do
  op <- mop
  nested $ do
    x <- mx
    y <- divClass "mt-1" my
    pure (x, op, y)

expr
  :: forall t m lit. (DomBuilder t m, MonadFix m, MonadHold t m, HasEditorEnv t m, PostBuild t m, UniverseSome (ExprTag lit))
  => VariableTag lit
  -> Editor t m lit
  -> Editor t m (Expr lit)
expr tag valField = Editor.newtypeDSum $ dsumEditor "Expression" universeF showOption $ \t -> nested . subEditor t
  where
    showOption :: Some (ExprTag a) -> Text
    showOption = foldSome $ \case
      ExprTag_Arithmetic2 -> "Formula"
      ExprTag_Balance     -> "Balance"
      ExprTag_Lit         -> "Literal"
      ExprTag_Receive     -> "Receiving"
      ExprTag_Send        -> "Sending"
      ExprTag_Var         -> "Variable"

    subEditor :: ExprTag lit x -> Editor t m x
    subEditor = \case
      ExprTag_Arithmetic2 -> arithmetic2 tag valField
      ExprTag_Balance     -> token
      ExprTag_Lit         -> valField
      ExprTag_Receive     -> token
      ExprTag_Send        -> token
      ExprTag_Var         -> varWithFixedTag tag

dataVar
  :: (DomBuilder t m, MonadFix m, MonadHold t m, HasEditorEnv t m, PostBuild t m)
  => VariableTag a -> Editor t m (Oracle a)
dataVar tag = Editor.iso Lens._Unwrapped' $ Editor.triple party (varWithFixedTag tag) duration $ \p v d -> do
  liftA3 (,,) (labeled "Address" p) (labeled "Variable" v) (labeled "Time" d)

-- TODO: This is awkward.
-- We don't actually allow editing the variable tag, so this signature isn't quite right
-- but everything depending on `var` seems to do this wrapping.
varWithFixedTag
  :: (DomBuilder t m, MonadFix m, MonadHold t m, HasEditorEnv t m, PostBuild t m)
  => VariableTag a -> Editor t m (Var a)
varWithFixedTag tag = (fmap . fmap . fmap) (Var tag) . var tag . fmap (\(Var _ name) -> name)

var
  :: (DomBuilder t m, MonadFix m, MonadHold t m, HasEditorEnv t m, PostBuild t m)
  => VariableTag a
  -> Editor t m Text
var tag a0 = do
  let
    none = (Nothing, "-- Variable --")
    opts vs = Map.fromList $ (none:) $ ffor (sort $ toList vs) $ \v -> (Just v, v)

  (_, vars) <- ask
  let vars' = ffor vars $ maybe mempty getConst . DMap.lookup tag

  ev <- switchHold never <=< dyn $ ffor vars' $ \vs -> do
    flip withHold (curry snd) =<< formDropdown a0 (opts vs) id

  pure $ fmap maybeField $ mkHold a0 ev

-- TODO: collisions?
letVar :: (Adjustable t m, MonadFix m, MonadHold t m, HasEditorEnv t m) => VariableTag a -> Hold t (Field e Identifier) -> m b -> m b
letVar tg di x = do
  l <- withHoldM (fmap justFieldValue di) holdDyn
  local (second $ liftA2 insert l) x
  where
    insert = maybe id $ insertName . Var tg
