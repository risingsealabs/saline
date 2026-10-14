{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Saline.App.Wallet.PasswordUtils where

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative(liftA2)
#endif
import Control.Error (hush)
import Control.Lens ((%~))
import Data.Char (isDigit, isLower, isUpper)
import Data.Foldable
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Reflex.Dom.Core

import Saline.App.Widgets
import Saline.Crypto (Password (Password))
import Saline.Utils (nonEmptyText)

data PasswordStrength = Low | Medium | High deriving stock (Eq, Ord, Show)

data PasswordRequirement = Length Int | HasUppercase | HasLowercase | HasDigit | HasSpecialChar
  deriving stock (Eq, Show)

passwordRequirements :: [PasswordRequirement]
passwordRequirements = [Length 8, HasUppercase, HasLowercase, HasDigit, HasSpecialChar]

confirmPasswordForm :: (DomBuilder t m, MonadHold t m, PostBuild t m) => m (Dynamic t (Maybe Password))
confirmPasswordForm = do
  aDyn <- passwordField "Password"
  bDyn <- passwordField "Confirm Password"
  let
    passwordDyn = ffor2 aDyn bDyn $ liftA2 $ \ea eb -> do
      a <- ea
      case Right a == eb of
        False -> Left ["Passwords must match"]
        True -> Right a

  renderRequirements passwordDyn
  pure $ ffor passwordDyn (>>= hush)

passwordFieldUnchecked :: (DomBuilder t m, MonadHold t m) => Text -> m (Dynamic t (Maybe Text))
passwordFieldUnchecked placeholder = do
  formField Nothing nonEmptyText $ \cfg -> cfg
    & inputElementConfig_elementConfig . elementConfig_initialAttributes %~ fold
      [ Map.insert "autofocus" ""
      , Map.insert "placeholder" placeholder
      , Map.insert "type" "password"
      ]

passwordField :: (DomBuilder t m, MonadHold t m) => Text -> m (Dynamic t (Maybe (Either [Text] Password)))
passwordField placeholder = do
  passwordDyn <- passwordFieldUnchecked placeholder
  pure $ ffor passwordDyn $ fmap $ \pass ->
      case filter (not . checkPasswordRequirement pass) passwordRequirements of
        [] -> Right $ Password pass
        xs -> Left $ fmap showRequirement xs

renderRequirements :: (DomBuilder t m, PostBuild t m) => Dynamic t (Maybe (Either [Text] b)) -> m ()
renderRequirements passwordDyn = do
  dyn_ $ ffor passwordDyn $ maybe blank $ \case
    Left reqs -> errorMessage $ do
      text "Must meet the following requirements"
      elAttr "ul" ("style" =: "list-style: inside") $ for_ reqs $ \req ->
        el "li" $ text req
    Right _ -> blank

showRequirement :: PasswordRequirement -> Text
showRequirement = \case
  Length n -> T.pack $ "At least " <> show n <> " characters"
  HasUppercase -> "Contains uppercase letter"
  HasLowercase -> "Contains lowercase letter"
  HasDigit -> "Contains digit"
  HasSpecialChar -> "Contains special character"

passwordStrength :: Password -> PasswordStrength
passwordStrength (Password password) =
  case length $ filter (checkPasswordRequirement password) passwordRequirements of
    0 -> Low
    1 -> Low
    2 -> Medium
    3 -> Medium
    _ -> High

checkPasswordRequirement :: Text -> PasswordRequirement -> Bool
checkPasswordRequirement password = \case
  Length n -> T.length password >= n
  HasUppercase -> T.any isUpper password
  HasLowercase -> T.any isLower password
  HasDigit -> T.any isDigit password
  HasSpecialChar -> T.any (\c -> not (isUpper c || isLower c || isDigit c)) password
