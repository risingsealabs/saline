{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
module Saline.App.Utils where

import Clay (SpaceBetween (spaceBetween), justifyContent)
import Control.Lens ((%~))
import Control.Monad ((<=<))
import Control.Monad.Fix
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Map qualified as Map
import Data.Proxy
import Data.Text (Text)
import Data.Traversable
import Obelisk.Generated.Static (static)
import Reflex.Dom.Core
import Witherable (Filterable (catMaybes))

import Saline.App.Layout
import Saline.Intents.Condition (Token (..))
import Saline.Utils

copoutTODO :: DomBuilder t m => Text -> m ()
copoutTODO err = text $ "Error: " <> err

choices_
  :: (Reflex t, Monad m)
  => [a]
  -> (a -> m (Event t ()))
  -> m (Event t a)
choices_ xs f = choices xs $ \a -> (a <$) <$> f a

choices
  :: (Reflex t, Monad m)
  => [a]
  -> (a -> m (Event t b))
  -> m (Event t b)
choices xs f = fmap leftmost $ for xs f

replaceSelf_ :: (Adjustable t m, MonadFix m, MonadHold t m) => step -> (step -> m (Event t step)) -> m ()
replaceSelf_ a0 f = void $ replaceSelf a0 $ (fmap . fmap) Left . f

replaceSelf :: (Adjustable t m, MonadFix m, MonadHold t m) => step -> (step -> m (Event t (Either step stop))) -> m (Event t stop)
replaceSelf a0 f = mdo
  (ev0, evev) <- runWithReplace (f a0) $ fmap f next
  ev <- switchHold ev0 evev
  let (next, stop) = fanEither ev
  pure stop

setAttribute :: InitialAttributes cfg => AttributeName -> Text -> Override cfg
setAttribute k v = initialAttributes %~ Map.insert k v

addClass :: InitialAttributes cfg => Text -> Override cfg
addClass c = initialAttributes %~ Map.alter (Just . maybe c (\x -> x <> " " <> c)) "class"

setInitialValue, setPlaceholder :: Text -> Override (InputElementConfig EventResult t s)
setPlaceholder = setAttribute "placeholder"
setInitialValue v = inputElementConfig_initialValue .~ v

setPreventDefault :: forall t s en. DomSpace s => EventName en -> Override (ElementConfig EventResult t s)
setPreventDefault n = elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy s) n (const preventDefault)

field
  :: (DomBuilder t m, MonadHold t m)
  => a
  -> (Text -> a)
  -> Override (InputElementConfig EventResult t (DomBuilderSpace m))
  -> m (Dynamic t a)
field val0 parse = fmap snd . field' val0 parse

field'
  :: (DomBuilder t m, MonadHold t m)
  => a
  -> (Text -> a)
  -> Override (InputElementConfig EventResult t (DomBuilderSpace m))
  -> m (InputElement EventResult (DomBuilderSpace m) t, Dynamic t a)
field' val0 parse overrideCfg = do
  ie <- inputElement $ overrideCfg def
  fmap (ie,) $ holdDyn val0 $ fmap parse $ _inputElement_input ie -- TODO: JS paste doesn't trigger change here, I had to dispatchEvent 'input' on JS

textAreaField
  :: (DomBuilder t m, MonadHold t m)
  => a
  -> (Text -> a)
  -> Override (TextAreaElementConfig EventResult t (DomBuilderSpace m))
  -> m (Dynamic t a)
textAreaField val0 parse = fmap snd . textAreaField' val0 parse

textAreaField'
  :: (DomBuilder t m, MonadHold t m)
  => a
  -> (Text -> a)
  -> Override (TextAreaElementConfig EventResult t (DomBuilderSpace m))
  -> m (TextAreaElement EventResult (DomBuilderSpace m) t, Dynamic t a)
textAreaField' val0 parse overrideCfg = do
  ie <- textAreaElement $ overrideCfg def
  fmap (ie,) $ holdDyn val0 $ fmap parse $ _textAreaElement_input ie

checkbox :: DomBuilder t m => Bool -> Override (InputElementConfig EventResult t (DomBuilderSpace m)) -> m (Event t Bool)
checkbox val0 override = do
  fmap _inputElement_checkedChange $ inputElement $ override $ def
    & inputElementConfig_initialChecked .~ val0
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "checkbox")

keyValue :: (DomBuilder t m, Show a) => Text -> a -> m ()
keyValue k v = row (justifyContent spaceBetween) $ traverse_ (el "div" . text) [k <> ":", tshow v]

holdUniqEv :: (Eq a, MonadHold t m, MonadFix m, Reflex t) => Event t a -> m (Event t a)
holdUniqEv ev = do
  d <- holdUniqDyn <=< holdDyn Nothing $ fmap Just ev
  pure $ catMaybes $ updated d

-- Get the RGB color for a token
tokenColor :: Token -> Text
tokenColor = \case
  BTC   -> "#F7931A"
  ETH   -> "#646566"
  USDC  -> "#2775ca"
  USDT  -> "#27a17c"
  SALT  -> "#009FC0"

-- Get the icon file for a token
tokenIconPath :: Token -> Text
tokenIconPath = \case
  BTC  -> $(static "img/icon_btc.svg")
  ETH  -> $(static "img/icon_eth.svg")
  USDC -> $(static "img/icon_usdc.svg")
  USDT -> $(static "img/icon_usdt.svg")
  SALT -> $(static "img/icon_salt.png")
