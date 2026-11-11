{-# LANGUAGE OverloadedStrings #-}
module Saline.App.Wallet.Setup (setup) where

import Control.Error (hush)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad (replicateM_, unless)
import Data.Bool (bool)
import Data.Functor (void)
import Data.Semigroup (First (First, getFirst))
import Data.Text (Text)
import Data.Text qualified as Text
import Language.Javascript.JSaddle (MonadJSM)
import Reflex.Dom.Core hiding (checkbox)
import Witherable qualified

import Saline.App.Assets
import Saline.App.Editor.Utils.Reflex
import Saline.App.Environment
import Saline.App.Storage (Storage)
import Saline.App.Storage qualified as Storage
import Saline.App.Utils
import Saline.App.Wallet.PasswordUtils
import Saline.App.Widgets
import Saline.Crypto
import Saline.Utils
import Saline.App.Tailwind

data Screen
  = Screen_Setup
  | Screen_ImportMnemonic
  | Screen_Warning
  | Screen_ViewMnemonic
  | Screen_ConfirmMnemonic Secret
  | Screen_SetPassword (Word, Word) Secret
  | Screen_Unlock (EncryptedSecret, (Salt, Checksum))

setup
  :: forall t m
   . (DomBuilder t m, MonadFix m, MonadHold t m, MonadJSM m, MonadJSM (Performable m), PerformEvent t m, PostBuild t m)
  => Env t -> m (Either SecretKey (Event t SecretKey))
setup env = do
  Storage.local >>= \case
    Left err -> fatal err --TODO: try to make this impossible
    Right s -> Storage.get s Storage.Entry_Secret >>= \case
      Left err -> fatal err --TODO: what now?
      Right Nothing -> go s Screen_Setup
      Right (Just encrypted) -> case _env_autoUnlock env of
        Nothing -> go s $ Screen_Unlock encrypted
        Just p -> decrypt encrypted p >>= \case
          -- The development password is only a convenience: wallets created
          -- with another password must still be unlockable after a reload.
          Left _ -> go s $ Screen_Unlock encrypted
          Right sk -> pure $ Left sk

  where
    go :: Storage -> Screen -> m (Either SecretKey (Event t SecretKey))
    go storage start = do
      divClass "h-full gap-8 pt-20 flex flex-col items-center" $ do
        logo "h-40"

        hearKey $ replaceSelf_ start $ \case
          Screen_Setup                -> screen "Getting started with Saline Wallet" setupScreen
          Screen_ImportMnemonic       -> step 0 2 "Enter Secret Recovery Phrase" importMnemonicScreen
          Screen_Warning              -> step 0 4 "Secret Recovery Phrase Warnings" $ const warningScreen
          Screen_ViewMnemonic         -> step 1 4 "Secret Recovery Phrase" $ const viewMnemnonicScreen
          Screen_ConfirmMnemonic s    -> step 2 4 "Confirm Secret Recovery Phrase" $ confirmMnemonicScreen s
          Screen_SetPassword (i, l) s -> step i l "Set Password" $ const $ tellKey =<< setPasswordScreen storage s
          Screen_Unlock encrypted     -> screen "Unlock wallet" $ tellKey =<< unlockScreen (decrypt encrypted)

    fatal err = Right never <$ text err
    hearKey = fmap (Right . fmap getFirst . snd) . runEventWriterT
    tellKey ev = never <$ tellEvent (fmap First ev)

    step idx len txt f = do
      progress idx (len - idx - 1)
      screen txt $ f (idx, len)

    screen txt a = do
      let titleClass = "text-2xl text-center px-6"
                    <> md "text-3xl"
      elClass "h1" titleClass $ text txt
      a

-- Screens --
setupScreen :: (DomBuilder t m, PostBuild t m) => m (Event t Screen)
setupScreen = buttons <* readMore
  where
    buttons = divClass "grid grid-cols-2 my-8 gap-4" $ do
      generateEv <- primaryButton "Create new Wallet" pure
      importEv <- primaryButton "Import existing Wallet" pure
      pure $ leftmost [Screen_ImportMnemonic <$ importEv, Screen_Warning <$ generateEv]

    readMore = elClass "p" "text-center text-xs text-zinc-400" $ do
      text "Read more about Saline "
      elAttr "a" ("href" =: "https://github.com/risingsealabs/saline-sdk" <> "class" =: "underline text-white") (text "here")

warningScreen :: (DomBuilder t m, MonadHold t m, PostBuild t m) => m (Event t Screen)
warningScreen = do
  let warningClass = "px-4 text-sm"
                  <> md "text-base"
  elClass "p" warningClass $
    text "Make sure that you read and understand why you need to write down your “Secret Recovery Phrase”"

  let box extra = divClass $ extra <> " max-w-96 p-6 gap-4 flex flex-row items-center"
      txtClass = "text-sm"
              <> md "text-base"
  divClass "flex flex-row justify-center gap-2" $ do

    box "bg-orange-600/10" $ do
      alert ""
      elClass "p" txtClass $
        text "This is the ONLY WAY that you can recall your wallet if you lose your password or access to your device."
    box "bg-green-600/10" $ do
      unlock ""
      elClass "p" txtClass $
        text "Write down your Secret Recovery Phrase and store it somewhere safe, NEVER share this code with anyone"

  formSubmitWithCheckbox Screen_ViewMnemonic $ divClass "max-w-96" $
    elClass "p" txtClass $
      text "I understand that I am responsible for storing my secret recovery phrase, and that it is the only way to recover my wallet"

viewMnemnonicScreen
  :: (DomBuilder t m, MonadHold t m, MonadIO m, MonadJSM (Performable m), PerformEvent t m, PostBuild t m)
  => m (Event t Screen)
viewMnemnonicScreen = do
  liftIO generateSecret >>= \case
    Left err -> never <$ copoutTODO err
    Right secret -> do
      let ws = mnemonicToWords $ _secret_mnemonic secret

      text "This is the phrase you use to recover this wallet "
      case length ws == 12 of
        False -> pure never
        True -> do
          (e, ()) <- elAttr' "div" ("class" =: "flex flex-col items-center rounded-lg py-4 bg-input-bg cursor-copy") $ do
            void $ mnemonicGrid $ \i -> do
              divClass "flex flex-row mx-8 my-1 gap-4" $ do
                let word = ws !! fromEnum i --TODO: this currently can't fail but we should make it type safe
                mnemonicWordLabel i
                text word
            divClass "pt-8 text-sm opacity-50" $ text "Click anywhere on this card to copy your phrase"

          -- Safari only allows to write to the clipboard within gesture event
          -- and apparently "click" isn't one, but "mousedown" is 🤷
          copyToClipboard $ Text.unwords ws <$ domEvent Mousedown e

          formSubmitWithCheckbox (Screen_ConfirmMnemonic secret) $
            text "I have saved my phrase somewhere safe"

confirmMnemonicScreen :: (DomBuilder t m, MonadHold t m, PostBuild t m) => Secret -> (Word, Word) -> m (Event t Screen)
confirmMnemonicScreen secret (idx, len) = do
  text "Verify that you saved your secret phrase"
  (elForm, mnemonicDyn) <- el' "form" mnemonicInput
  let evPress = keyCodeLookup . fromIntegral <$> domEvent Keypress elForm
      evEnter = Witherable.filter (== Enter) evPress
      savedPhrase = ffor mnemonicDyn $ fmap $ \m -> case m == _secret_mnemonic secret of
        False -> Left "Mnemonic doesn't match"
        True -> Right secret
      hushedSecret = ffor savedPhrase (>>= hush)
      evEnterSecret = Witherable.catMaybes $ current hushedSecret <@ evEnter
      nextStep = Screen_SetPassword (idx + 1, len)

  runWithReplace_ blank $ ffor (updated savedPhrase) $ maybe blank $ either (errorMessage . text) $ const blank
  ev <- formSubmit "Next" hushedSecret
  pure $ leftmost [ nextStep <$> ev
                  , nextStep <$> evEnterSecret
                  ]

importMnemonicScreen :: (DomBuilder t m, MonadHold t m, PostBuild t m) => (Word, Word) -> m (Event t Screen)
importMnemonicScreen (idx, len) = do
  text "Enter your secret 12 word phrase"
  (elForm, mnemonicDyn) <- el' "form" mnemonicInput
  let evPress = keyCodeLookup . fromIntegral <$> domEvent Keypress elForm
      evEnter = Witherable.filter (== Enter) evPress
      evEnterMnemonics = Witherable.catMaybes $ current mnemonicDyn <@ evEnter

  ev <- formSubmit "Import" mnemonicDyn
  pure $ ffor (leftmost [ev, evEnterMnemonics]) $ Screen_SetPassword (idx + 1, len) . secretFromMnemonic

setPasswordScreen :: (DomBuilder t m, MonadHold t m, MonadJSM m, PostBuild t m) => Storage -> Secret -> m (Event t SecretKey)
setPasswordScreen storage sec = do
  text "You will use this to unlock your wallet"
  passEv <- formSubmit "Confirm" =<< confirmPasswordForm
  ((), initEv) <- runWithReplace blank $ ffor passEv $ runExceptT . initializeWallet storage (_secret_key sec)
  let (errEv, okEv) = fanEither initEv
  runWithReplace_ blank $ ffor errEv copoutTODO
  pure okEv

unlockScreen :: (DomBuilder t m, MonadHold t m, PostBuild t m) => (Password -> m (Either Text SecretKey)) -> m (Event t SecretKey)
unlockScreen unlockWithPass = do
  passDyn <- passwordFieldUnchecked "Password"
  passEv <- formSubmit "Unlock" $ ffor passDyn $ fmap Password
  ((), checkEv) <- runWithReplace blank $ ffor passEv unlockWithPass
  let (errEv, okEv) = fanEither checkEv
  void $ errorMessage $ runWithReplace blank $ ffor errEv text
  pure okEv

-- Utils --
progress :: DomBuilder t m => Word -> Word -> m ()
progress behind ahead = do
  let
    circle extra = divClass ("rounded-full size-6 " <> extra) blank

  divClass "flex flex-row gap-8" $ do
    replicateM_ (fromEnum behind) $ circle "bg-gray-500"
    circle "bg-white"
    replicateM_ (fromEnum ahead) $ circle "border-2 border-white"

mnemonicInput :: (DomBuilder t m, MonadHold t m) => m (Dynamic t (Maybe Mnemonic))
mnemonicInput = do
  wordDyns <- mnemonicGrid mnemonicWordField
  let
    wordsDyn = sequence wordDyns
    sentenceDyn = fmap sequence wordsDyn
  pure $ ffor sentenceDyn $ \s -> do
    ws <- s
    hush $ mnemonicFromWords ws

mnemonicGrid :: DomBuilder t m => (Word -> m a) -> m [a]
mnemonicGrid slot =
  elAttr "div" ("id" =: "mnemonics" <> "class" =: "grid grid-cols-3 gap-2") $ ireplicateM 12 slot

mnemonicWordField :: (DomBuilder t m, MonadHold t m) => Word -> m (Dynamic t (Maybe Text))
mnemonicWordField idx = do
  divClass "flex flex-row items-center rounded-lg bg-input-bg p-2 gap-2" $ do
    mnemonicWordLabel idx
    let autoFocusAttr = bool id (setAttribute "autofocus" "") (idx == 0)
    formField Nothing (nonEmptyText . Text.strip) $ addClass "w-32 p-0 mnemonicsField" . setAttribute "style" "box-shadow: none" . setAttribute "onpaste" "window['saline'].pasteMnemonic(this); return false;" . setAttribute "autocorrect" "off" . setAttribute "autocapitalize" "off" . autoFocusAttr

mnemonicWordLabel :: DomBuilder t m => Word -> m ()
mnemonicWordLabel i = divClass "min-w-6 text-right" $ text $ tshow $ i + 1

decrypt :: Monad m => (EncryptedSecret, (Salt, Checksum)) -> Password -> m (Either Text SecretKey)
decrypt (m, (salt, c)) pass = do
  runExceptT $ do
    encryptionKey <- happyPath $ hashPassword pass salt
    sk <- happyPath $ decryptSecret encryptionKey m
    unless (c == checksum sk salt) $
      throwError "Wrong password"
    pure sk

initializeWallet :: (MonadError Text m', MonadJSM m') => Storage -> SecretKey -> Password -> m' SecretKey
initializeWallet st sk pass = do
  salt <- liftIO genSalt
  encryptionKey <- happyPath $ hashPassword pass salt
  bs <- happyPath $ encryptSecret encryptionKey sk
  Storage.nuke st
  (bs', (salt', _)) <- happyWrite st Storage.Entry_Secret (bs, (salt, checksum sk salt))
  encryptionKey' <- happyPath $ hashPassword pass salt'
  happyPath $ decryptSecret encryptionKey' bs'

-- TODO: carefully go over each usage site
happyPath :: MonadError e m' => Either e a -> m' a
happyPath = either throwError pure

-- TODO: carefully go over each usage site
happyWrite :: (MonadError Text m', MonadJSM m') => Storage -> Storage.Entry a -> a -> m' a
happyWrite st k v = do
  Storage.set st k v
  Storage.get st k >>= \case
    Left err -> throwError err
    Right Nothing -> throwError "Write failed"
    Right (Just a) -> pure a
