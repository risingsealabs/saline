{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Saline.App.Editor.Field where

import Clay qualified
import Control.Arrow ((&&&))
import Control.Lens qualified as Lens
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.Writer
import Data.Bifunctor
import Data.Bool
import Data.Dependent.Sum
import Data.Foldable
import Data.Functor.Identity
import Data.GADT.Compare
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.Monoid
import Data.Some
import Data.Text (Text)
import Data.Text qualified as Text
import Reflex.Dom.Core

import Saline.Utils.Field
import Saline.Utils
import Saline.App.Editor.Utils.Editor (Editor, Editor')
import Saline.App.Editor.Utils.Reflex
import Saline.App.Editor.Utils.Reflex.Dom
import Saline.App.Editor.Widgets
import Saline.App.Layout
import Saline.App.Utils
import Saline.App.Widgets
import Saline.App.Editor.Utils.Editor qualified as Editor

conditionField
  :: (DomBuilder t m, MonadFix m, MonadHold t m)
  => Editor' t m e a
  -> Editor' t m e a
conditionField f f0 = mdo
  -- TODO: strictness gremlins:
  --   switching the next two lines triggers a MonadFix loop: <interactive>: cyclic evaluation in fixIO
  let moated = elHoldAttr "div" $ ffor (mkHold (maybeField f0) ev) $ style . moatCss . Just . fieldBorderColor
  h <- moated $ f f0
  (_, ev) <- unHold h
  pure h

fieldBorderColor :: Field e a -> Clay.Color
fieldBorderColor (Field f) = colors & case f of
  Left _ -> _colorCoding_err
  Right input -> case input of
    Nothing -> _colorCoding_missing
    Just valid -> case getAlt (execWriter valid) of
      Nothing -> _colorCoding_ok
      Just hint -> case hint of
        Hint_Warning -> _colorCoding_warn

constantField :: Applicative m => a -> m (Hold t (Field Text a))
constantField = pure . pure . pure

nonEmptyInput
  :: (DomBuilder t m, MonadFix m, MonadHold t m)
  => (Text -> Field e a) -> Text -> m (Hold t (Field e a))
nonEmptyInput parse txt0 = mdo
  let
    s = style . borderCss (Clay.px 1) Clay.solid . fieldBorderColor
    val0 = parse txt0

  (ie, _) <- formField' val0 parse $ \cfg -> cfg
    & inputElementConfig_initialValue .~ txt0
    & addClass "w-24"
    -- & inputElementConfig_elementConfig . elementConfig_initialAttributes <>~ s val0
    & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ ffor fieldEv (fmap Just . s)

  fieldHold@(_, fieldEv) <- unHold $ ffor (mkHold txt0 (_inputElement_input ie)) $ \case
    "" -> unfilled
    txt -> parse txt

  pure $ Hold $ pure fieldHold

contextDependentField
  :: (Adjustable t m, MonadHold t m, MonadFix m)
  => (kv -> k)
  -> ((forall x. x -> Maybe x) -> kv -> kmv)
  -> (k -> kmv)
  -> Editor' t m e k
  -> (kmv -> m (Hold t (Field e a)))
  -> Editor t m e kv a
contextDependentField viewKey overValue fromNewKey keyField valField kv0 = do
  (_, kEv) <- unHold =<< keyField (fmap viewKey kv0)

  let
    v0 = ffor kv0 $ overValue Just
    vEv = ffor kEv $ \mk -> ffor mk fromNewKey

  (f0, fEv) <- unHold <=< fmap flattenHold $ runHold $ maybe (pure $ pure unfilled) valField <$> mkHold v0 (fmap justFieldValue vEv)
  pure $ mkHold f0 $ leftmost [unfilled <$ kEv, fEv]

-- TODO: should this use prisms?
pairField
 :: (DomBuilder t m, MonadFix m, MonadHold t m)
  => Editor' t m e k
  -> (k -> Editor' t m e v)
  -> Editor' t m e (k, v)
pairField keyField valField = contextDependentField
  fst
  second
  (, Nothing)
  keyField
  $ \(k, mv) -> (fmap . fmap . fmap) (k,) $ valField k mv

dsumField
  :: (DomBuilder t m, MonadFix m, MonadHold t m)
  => Editor' t m e (Some k)
  -> (forall x. k x -> Editor' t m e x)
  -> Editor' t m e (DSum k Identity)
dsumField keyField valField = contextDependentField
  someKey
  (\f -> mapValue (f . runIdentity))
  (fromKey (const Nothing))
  keyField
  $ \(k :=> mv) -> (fmap . fmap . fmap) ((k :=>) . Identity) $ valField k mv

dropdownPair
  :: ( DomBuilder t m, MonadFix m, MonadHold t m
     , Ord k
     )
  => Bool
  -> Map (Maybe k) Text
  -> (k -> Editor' t m e v)
  -> Editor' t m e (k, v)
dropdownPair autoFocus opts = pairField $ \k ->
  (fmap . fmap) maybeField $ formDropdown k opts $ bool id withAutoFocus autoFocus
  where withAutoFocus = selectElementConfig_elementConfig . elementConfig_initialAttributes .~ ("autofocus" =: "")

maybeEditor
  :: (DomBuilder t m, MonadFix m, MonadHold t m)
  => Text
  -> (Maybe () -> Text)
  -> Editor t m e a a
  -> Editor t m e (Maybe a) (Maybe a)
maybeEditor placeholder mkLabel e = Editor.iso (Lens.iso dsumToMaybe maybeToDSum) $ dropdownDSum False opts $ \case
  NothingTag -> Editor.unit
  JustTag -> e
  where
    opts = fold
      [ Nothing =: Text.unwords ["--", placeholder, "--"]
      , Just (Some JustTag) =: mkLabel (Just ())
      , Just (Some NothingTag) =: mkLabel Nothing
      ]

dropdownDSum
  :: ( DomBuilder t m, MonadFix m, MonadHold t m
     , GCompare k
     )
  => Bool
  -> Map (Maybe (Some k)) Text
  -> (forall x. k x -> Editor' t m e x)
  -> Editor' t m e (DSum k Identity)
dropdownDSum autoFocus opts = dsumField $ \k ->
  (fmap . fmap) maybeField $ formDropdown k opts $ bool id withAutoFocus autoFocus
  where withAutoFocus = selectElementConfig_elementConfig . elementConfig_initialAttributes .~ ("autofocus" =: "")

dsumEditor
  :: ( DomBuilder t m, MonadFix m, MonadHold t m
     , GCompare k
     )
  => Text
  -> [Some k]
  -> (Some k -> Text)
  -> (forall x. k x -> Editor' t m e x)
  -> Editor' t m e (DSum k Identity)
dsumEditor name keys showOption subEditor = dropdownDSum False options subEditor
  where
    none = (Nothing, "-- " <> name <> " --")
    options = Map.fromList $ (none:) $ ffor keys $ Just &&& showOption

-- TODO: impl way too complicated - simplify
expanding
  :: (DomBuilder t m, MonadFix m, MonadHold t m)
  => (Maybe a -> m (Hold t (Field e  a)))
  -> ([a]     -> m (Hold t (Field e [a])))
expanding f xs0 = column blank $ mdo
  nonce <- holdUniqDyn <=< foldDyn max (length xs0) $ ffor stateEv $ length . ffilter (isJust . justFieldValue)

  let
    keys0 = Map.fromList $ zip [0..] $ fmap Just xs0 <> [Nothing]
    nonceEv = ffor (updated nonce) $ \n -> Map.singleton n (Just Nothing)

  listDyn <- listHoldWithKey keys0 nonceEv $ \_ x -> divClass "mt-2" $ do
    h <- f x
    (_, ev) <- unHold h
    pure ev

  let patchesEv = switch $ fmap mergeMap $ current listDyn

  stateEv <- fmap updated $ foldDyn appEndo (fmap maybeField keys0) $ ffor patchesEv $
    Map.foldMapWithKey (\k -> Endo . Map.insert k)

  pure $ mkHold (pure xs0) $ ffor stateEv $ sequenceA . reverse . dropWhile isUnfilled . reverse . toList

data ColorCoding a = ColorCoding
  { _colorCoding_ok      :: a
  , _colorCoding_missing :: a
  , _colorCoding_warn    :: a
  , _colorCoding_err     :: a
  } deriving stock (Eq, Ord, Functor, Foldable, Traversable)

colors :: ColorCoding Clay.Color
colors = ColorCoding
  { _colorCoding_ok      = Clay.green
  , _colorCoding_missing = Clay.white
  , _colorCoding_warn    = Clay.goldenrod
  , _colorCoding_err     = Clay.red
  }

colorDescriptions :: ColorCoding Text
colorDescriptions = ColorCoding
  { _colorCoding_ok      = "Ok"
  , _colorCoding_missing = "Missing"
  , _colorCoding_warn    = "Warning"
  , _colorCoding_err     = "Error"
  }
instance Applicative ColorCoding where
  pure a = a <$ colors

  (<*>) :: forall a b. ColorCoding (a -> b) -> ColorCoding a -> ColorCoding b
  a <*> b = ColorCoding
    { _colorCoding_ok      = f _colorCoding_ok
    , _colorCoding_missing = f _colorCoding_missing
    , _colorCoding_warn    = f _colorCoding_warn
    , _colorCoding_err     = f _colorCoding_err
    }
    where
      f :: (forall x. ColorCoding x -> x) -> b
      f fld = fld a (fld b)
