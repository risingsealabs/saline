{-# LANGUAGE OverloadedStrings #-}
module Saline.App.Editor.Render where

import Clay qualified
import Control.Arrow ((>>>))
import Control.Monad.Fix
import Data.Bool
import Data.Dependent.Sum
import Data.Foldable
import Data.Functor.Identity
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Tuple
import Reflex.Dom.Core hiding (Alt, list, tag)

import Saline.App.Layout
import Saline.Crypto
import Saline.Intents (Intent)
import Saline.Intents.Condition
import Saline.Utils hiding (timestamp)
import Saline.Utils qualified as Utils

hexWrapper :: DomBuilder t m => m a -> m a
hexWrapper = elAttr "div" $ style $ do
  Clay.fontFamily [] [Clay.monospace]
  Clay.width (Clay.px 200)
  Clay.overflow Clay.hidden
  Clay.textOverflow Clay.overflowEllipsis

address :: (DomBuilder t m, PostBuild t m) => Dynamic t PublicKey -> m ()
address = hexWrapper . dynText . fmap addressEllipsis

amount :: DomBuilder t m => Amount -> m ()
amount = text . tshow

word :: DomBuilder t m => Word -> m ()
word = text . tshow

token :: DomBuilder t m => Token -> m ()
token = text . tshow

variable :: DomBuilder t m => Var a -> m ()
variable (Var _ name) = text $ "$" <> name

timestamp :: DomBuilder t m => Timestamp -> m ()
timestamp = text . Utils.timestamp . realToFrac

-- TODO: can't seem to write with pattern synonyms as inference breaks on ExprTag_Arithmetic2
expr :: forall t m a. (DomBuilder t m, PostBuild t m) => (a -> m ()) -> Expr a -> m ()
expr render = unNewtypeDSum >>> \case
  ExprTag_Balance :=> Identity t -> token t *> text " balance"
  ExprTag_Lit :=> Identity l -> render l
  ExprTag_Var :=> Identity v -> variable v
  ExprTag_Arithmetic2 :=> Identity (Binary x op y) -> do
    text "("
    expr render x
    text $ symbol op
    expr render y
    text ")"
  ExprTag_Receive :=> Identity f -> flow "Incoming" f
  ExprTag_Send :=> Identity f -> flow "Outgoing" f

flow :: DomBuilder t m => Text -> Token -> m ()
flow act t = do
  text $ act <> " "
  token t
  text " "

movement :: DomBuilder t m => (p -> m ()) -> (Text, Text, Text) -> Role -> p -> m () -> m ()
movement renderParty (sourceL, sinkL, assetsL) hostRole party assets = bipartite (sourceL, source) (sinkL, sink) (assetsL, assets)
  where (source, sink) = bool swap id (hostRole == Role_Source) (text "<intent>", renderParty party)

withLabel :: DomBuilder t m => Text -> (Text, m a) -> m a
withLabel cls (lbl, a) = divClass cls $ text (lbl <> ": ") *> a

bipartite :: DomBuilder t m => (Text, m ()) -> (Text, m ()) -> (Text, m ()) -> m ()
bipartite party1 party2 object = divClass "flex flex-col" $ do
  withLabel "flex flex-row" party1
  withLabel "flex flex-row" party2
  withLabel "flex flex-col" object

transfer :: DomBuilder t m => (a -> m ()) -> (p Address -> m ()) -> Transfer a p -> m ()
transfer renderAssets renderParty (Transfer party role as) = movement renderParty ("From", "To", "Transfer") role party $ renderAssets as

transferRights :: DomBuilder t m => (Condition e p l -> m ()) -> (p Address -> m ()) -> (Transfer (Condition e p l, p Address) p) -> m ()
transferRights go renderParty (Transfer party role (c, issuer)) = movement renderParty ("Seller", "Buyer", "Contract") role party $ do
  withLabel "" ("Issuer", renderParty issuer)
  go c

issuance :: DomBuilder t m => (Condition e p l -> m ()) -> (p Address -> m ()) -> Issuance e p l -> m ()
issuance go renderParty (Transfer party hostRole terms) = movement renderParty ("Issuer", "Holder", "Terms") hostRole party $ go terms

contract :: DomBuilder t m => (Condition e p l -> m ()) -> (m (), m ()) -> Condition e p l -> m ()
contract go (issuer, holder) terms = bipartite ("Issuer", issuer) ("Holder", holder) ("Terms", go terms)

counterparty :: (DomBuilder t m, PostBuild t m) => Address -> m ()
counterparty v = el "div" $ text "Counterparty is " *> address (pure v)

sig :: (DomBuilder t m, PostBuild t m) => Address -> m ()
sig v = el "div" $ text "Signed by " *> address (pure v)

finite :: DomBuilder t m => Uses -> m a -> m a
finite u = wrapper $ "Limited to " <> quantity "use" u
  where quantity = flip simplePlural

temporary :: DomBuilder t m => (Duration, Bool) -> m a -> m a
temporary (d, after) = wrapper $ Text.unwords ["Available", bool "before" "after" after, Utils.timestamp (realToFrac d)]

wrapper :: DomBuilder t m => Text -> m a -> m a
wrapper t a = text t *> a

conditionInstalled :: (DomBuilder t m, PostBuild t m) => Intent -> m ()
conditionInstalled = condition expr (expr (address . pure)) $ \go xs header -> do
  divClass "flex flex-col" $ do
    el "div" $ text header
    for_ xs go

holderRole :: DomBuilder t m => (Text, Text) -> Role -> m ()
holderRole (source, sink) = text . \case
  Role_Source -> source
  Role_Sink -> sink

restriction :: DomBuilder t m => (e Amount -> m ()) -> (Binary Relation (e Amount)) -> m ()
restriction f (Binary lhs rel rhs) = do
  f lhs
  text $ " " <> ppRelation rel <> " "
  f rhs

list :: DomBuilder t m => (elem -> m ()) -> List elem -> Text -> m ()
list go ps header = column blank $ do
  case ps of
    List_Lit xs -> list' go xs header
    List_For i _ a -> do
      el "div" $ do
        text header
        indented $ do
          text "for each "
          text i
          go a

list' :: DomBuilder t m => (elem -> m ()) -> [elem] -> Text -> m ()
list' go ps header = column blank $ do
  el "div" $ text header
  for_ ps go

--TODO: generate multi-transaction scenarios?
conditionConcrete :: (DomBuilder t m, PostBuild t m) => Condition Identity Identity [] -> m ()
conditionConcrete = fix $ \go -> dsum $ \case
  ConditionTag_All -> column blank . traverse_ (el "div" . go) . runIdentity
  ConditionTag_Counterparty -> counterparty . runIdentity
  ConditionTag_Finite -> \(Identity (_, c)) -> go c
  ConditionTag_Temporary -> \(Identity (_, _, c)) -> go c
  ConditionTag_Any -> \(Identity (k, ps)) -> go $ Condition_All $ take (fromEnum k) ps -- TODO: add back shuffle
  ConditionTag_Restriction -> restriction (amount . runIdentity) . runIdentity
  ConditionTag_Signature -> sig . runIdentity

conditionSymbolic :: (DomBuilder t m, PostBuild t m) => Condition Expr Expr [] -> m ()
conditionSymbolic = condition expr (expr (address . pure)) list'

condition
  :: (DomBuilder t m, PostBuild t m)
  => (forall x. (x -> m ()) -> e x -> m ())
  -> (p Address -> m ())
  -> ((Condition e p l -> m ()) -> l (Condition e p l) -> Text -> m ())
  -> Condition e p l
  -> m ()
condition renderExpr _ renderList = go
  where
    moat = divClass "m-2 p-2 border border-dotted border-saline"

    go = (moat .) $ dsum $ \case
      ConditionTag_All -> \(Identity ps) -> renderList go ps "all"
      ConditionTag_Any -> \(Identity (k, ps)) -> renderList go ps $ "any " <> tshow k
      ConditionTag_Counterparty -> counterparty . runIdentity
      ConditionTag_Finite -> \(Identity (n, c)) -> finite n $ go c
      ConditionTag_Temporary -> \(Identity (d, a, c)) -> temporary (d, a) $ go c
      ConditionTag_Restriction -> restriction (renderExpr amount) . runIdentity
      ConditionTag_Signature -> sig . runIdentity
