{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module Saline.Crypto where

import Prelude hiding (id, (.))
import Control.Category (id, (.))

import Control.Error (note)
import Control.Monad
import Control.Monad.Error.Class (MonadError (..))
import Crypto.Error (CryptoFailable (..))
import Data.Aeson hiding (encode)
import Data.Aeson qualified as Aeson hiding (encode)
import Data.Aeson.Types (Parser, parseFail)
import Data.Base16.Types
import Data.Bool
import Data.ByteArray qualified as ByteArray
import Data.ByteArray.Sized (SizedByteArray (unSizedByteArray), sizedByteArray, unsafeSizedByteArray)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (decodeBase16', encodeBase16)
import Data.Char (isHexDigit)
import Data.Containers.ListUtils (nubOrd)
import Data.Either (fromRight)
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Semigroup (Endo (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding qualified as Text
import Data.UUID (UUID)
import Data.Word (Word32)
import GHC.Generics
import GHC.TypeLits (KnownNat, TypeError, natVal)
import GHC.TypeLits qualified as TypeLits
import Obelisk.Route
import Text.ParserCombinators.ReadPrec (ReadPrec)
import Text.Read (Read(readPrec))

import Saline.Core.Types (SalineAddr)
import Saline.Utils

#ifdef ghcjs_HOST_OS
import Control.Lens (iso, lazy, strict, view)
import Crypto.Hash (Digest, SHA256, digestFromByteString)
import Data.Digest.Pure.SHA (bytestringDigest, sha256)
import Data.Foldable (toList)
import Data.String (IsString)
#else
import Crypto.BLST qualified as BLST
import Crypto.BLST.Internal.Bindings.Types qualified as BLST
import Crypto.BLST.Internal.Classy qualified as BLST
import Crypto.Cipher.AES
import Crypto.Cipher.Types (BlockCipher (ctrCombine), Cipher (cipherInit), nullIV)
import Crypto.Encoding.BIP39 qualified as BIP39
import Crypto.Encoding.BIP39.English qualified as BIP39
import Crypto.KDF.Argon2 (Variant (Argon2id))
import Crypto.KDF.Argon2 qualified as Argon2
import Crypto.Hash (Digest, SHA256)
import Crypto.MAC.HMAC (HMAC (..), hmac)
import Crypto.Random.Entropy (getEntropy)
import Data.Bifunctor
import Data.ByteArray (ScrubbedBytes)
import Data.ByteArray.Sized qualified as Sized
import Data.String (IsString (fromString))
import Data.UUID.V4 (nextRandom)
import Foreign.C (CUInt)
import GHC.Exts qualified as GHC
#endif

type BIP32Child = Word32

--TODO: don't export constructor
newtype Mnemonic = Mnemonic
#ifdef ghcjs_HOST_OS
  { getMnemonic :: Text }
#else
  { getMnemonic :: BIP39.MnemonicSentence (BIP39.MnemonicWords 128) }
#endif
  deriving stock (Eq, Show)

--TODO: don't export constructor
newtype SecretKey = SecretKey
#if defined(ghcjs_HOST_OS)
  { getSecretKey :: ByteString }
#else
  { getSecretKey :: BLST.SecretKey }
#endif
  deriving stock Eq

data Secret = Secret
  { _secret_mnemonic :: Mnemonic
  , _secret_key :: SecretKey
  }

type SecretsError = 'TypeLits.Text "Avoid serialization of secrets to ensure they never leave memory"

-- TODO: once we are on ghc 9.8+, use Unsatisfiable instead
instance TypeError SecretsError => Show SecretKey where showsPrec = error "unreachable"
instance TypeError SecretsError => ToJSON SecretKey where toJSON = error "unreachable"

newtype Password = Password Text --TODO: don't export constructor
  deriving stock Eq
  deriving newtype IsString

instance TypeError SecretsError => Show Password where showsPrec = error "unreachable"
instance TypeError SecretsError => ToJSON Password where toJSON = error "unreachable"

getEntropySized :: forall n. KnownNat n => IO (SizedByteArray n ByteString)
getEntropySized = fmap unsafeSizedByteArray $ do
  let bytes = fromInteger $ natVal $ Proxy @n
#ifdef ghcjs_HOST_OS
  unsafeHex2bytestring "getEntropySized" <$> js_getEntropy (bytes * 8)
foreign import javascript unsafe "bip39.mnemonicToEntropy(bip39.generateMnemonic($1))"
  js_getEntropy :: Word -> IO Text
#else
  getEntropy bytes
#endif

#ifdef ghcjs_HOST_OS
--TODO: don't assume english?
#else
dictionary :: BIP39.Dictionary
dictionary = BIP39.english
#endif

-- TODO: this really should always work, get rid of Either
generateSecret :: IO (Either Text Secret)
#ifdef ghcjs_HOST_OS
foreign import javascript unsafe "bip39.generateMnemonic(128)"
  js_generateMnemonic :: IO Text
generateSecret = do
  m <- js_generateMnemonic
  pure $ Right $ secretFromMnemonic $ Mnemonic m
#else
generateSecret = fmap secretFromEntropy getEntropySized

secretFromEntropy :: SizedByteArray 16 ByteString -> Either Text Secret
secretFromEntropy = fmap (secretFromMnemonic . Mnemonic . BIP39.entropyToWords) . first tshow . BIP39.toEntropy @128
#endif

secretFromMnemonic :: Mnemonic -> Secret
secretFromMnemonic m@(Mnemonic sentence) = Secret m $ secretKeyFromSeed $
#ifdef ghcjs_HOST_OS
  unsafeHex2sizedBytestring "secretFromMnemonic" $ js_mnemonicToSeedSync sentence
foreign import javascript unsafe "saline.bytestring2hex(bip39.mnemonicToSeedSync($1))"
  js_mnemonicToSeedSync :: Text -> Text
#else
  unsafeSizedByteArray $ ByteArray.convert $ BIP39.sentenceToSeed sentence dictionary ""
#endif

secretKeyFromSeed :: SizedByteArray 64 ByteString -> SecretKey
secretKeyFromSeed = SecretKey . deriveMasterEIP2333
  where
    deriveMasterEIP2333 =
#ifdef ghcjs_HOST_OS
      unsafeHex2bytestring "secretKeyFromSeed" . js_deriveMasterSK . sizedBytestring2hex
foreign import javascript unsafe "saline.roundtripBinary(bls_hd_key.deriveMasterSK)($1)"
  js_deriveMasterSK :: Text -> Text
#else
      BLST.deriveMasterEIP2333
#endif

#ifdef ghcjs_HOST_OS
deriveChildEIP2333 :: SecretKey -> BIP32Child -> SecretKey
deriveChildEIP2333 sk i =
  SecretKey $ unsafeRoundtripBinary "deriveChildEIP2333" (js_deriveChildEIP2333 i) $ getSecretKey sk
foreign import javascript unsafe "saline.bytestring2hex(bls_hd_key.deriveChildSK(saline.hex2bytestring($2), $1))"
  js_deriveChildEIP2333 :: Word32 -> Text -> Text
#else
deriveChildEIP2333 :: SecretKey -> CUInt -> SecretKey
deriveChildEIP2333 sk i =
  SecretKey $ BLST.deriveChildEIP2333 (getSecretKey sk) i
#endif

mnemonicToWords :: Mnemonic -> [Text]
#ifdef ghcjs_HOST_OS
mnemonicToWords = Text.words . getMnemonic
#else
mnemonicToWords = convert . BIP39.mnemonicSentenceToMnemonicPhrase dictionary . getMnemonic
  where convert = fmap (Text.pack . GHC.toList) . GHC.toList
#endif

mnemonicFromWords :: [Text] -> Either Text Mnemonic
#ifdef ghcjs_HOST_OS
mnemonicFromWords = Right . Mnemonic . Text.intercalate " "
#else
mnemonicFromWords ws = do
  p <- first tshow $ BIP39.mnemonicPhrase $ fmap (fromString . Text.unpack) ws
  bimap tshow Mnemonic $ BIP39.mnemonicPhraseToMnemonicSentence dictionary p
#endif

type Salt = SizedByteArray 32 ByteString
type EncryptionKey = SizedByteArray 32 ByteString
type EncryptedSecret = SizedByteArray 32 ByteString

sizeOfByteArray :: forall n ba. KnownNat n => Proxy (SizedByteArray n ba) -> Int
sizeOfByteArray Proxy = fromInteger $ natVal $ Proxy @n

genSalt :: IO Salt
genSalt = getEntropySized

genUUID :: IO UUID
genUUID =
#ifdef ghcjs_HOST_OS
  fmap tread js_randomUUID
foreign import javascript unsafe "crypto.randomUUID()"
  js_randomUUID :: IO Text
#else
  nextRandom
#endif

hoistFailable :: MonadError Text m => CryptoFailable a -> m a
hoistFailable = \case
  CryptoFailed e -> throwError $ tshow e
  CryptoPassed a -> pure a

hashPassword :: forall out. out ~ SizedByteArray 32 ByteString => Password -> Salt -> Either Text out
hashPassword (Password pass) salt = do
  let p = encodeUtf8 pass
#ifdef ghcjs_HOST_OS
  let h = view strict $ bytestringDigest $ sha256 $ view lazy (p <> unSizedByteArray salt)
#else
  let options = Argon2.defaultOptions { Argon2.variant = Argon2id }
  h <- hoistFailable $ Argon2.hash @ScrubbedBytes options (ByteArray.convert p) salt $ sizeOfByteArray $ Proxy @out
#endif
  note "hashPassword: hash has wrong size - this shouldn't happen" $ sizedByteArray h

type Checksum = Digest SHA256
checksum :: SecretKey -> Salt -> Checksum
#if defined(ghcjs_HOST_OS)
checksum sk salt = fromMaybe (error "checksum: invalid size") $ digestFromByteString $ view strict $ bytestringDigest $ sha256 $ view lazy (getSecretKey sk <> unSizedByteArray salt)
#else
checksum sk salt = hmacGetDigest $ hmac (BLST.serializeSk $ getSecretKey sk) (unSizedByteArray salt)
#endif

#if defined(ghcjs_HOST_OS)
--TODO: not this
xorSecret :: (KnownNat n, ByteArray.ByteArrayAccess a2,  ByteArray.ByteArrayAccess b, ByteArray.ByteArray ba) => a2 -> b -> Either Text (SizedByteArray n ba)
xorSecret ek sk = note "encryptSecret: wrong size - this shouldn't happen" $ sizedByteArray $ ByteArray.xor ek sk

encryptSecret :: EncryptionKey -> SecretKey -> Either Text EncryptedSecret
encryptSecret ek = xorSecret ek . getSecretKey

decryptSecret :: EncryptionKey -> EncryptedSecret -> Either Text SecretKey
decryptSecret ek = fmap (SecretKey . unSizedByteArray) . xorSecret @32 ek

#else

-- TODO: should we really be calling cipherInit everytime?
scrubbedCTR :: KnownNat n => EncryptionKey -> SizedByteArray n ScrubbedBytes -> Either Text (SizedByteArray n ScrubbedBytes)
scrubbedCTR ek input = do
  c <- first tshow $ hoistFailable $ cipherInit @AES256 $ unSizedByteArray ek
  note "scrubbedCTR: ctr changed size - this shouldn't happen" $ sizedByteArray $ ctrCombine c nullIV $ unSizedByteArray input

encryptSecret :: EncryptionKey -> SecretKey -> Either Text EncryptedSecret
encryptSecret ek = fmap Sized.convert . scrubbedCTR ek . BLST.serializeSk . getSecretKey

decryptSecret :: EncryptionKey -> EncryptedSecret -> Either Text SecretKey
decryptSecret ek = fmap (SecretKey . BLST.deserializeSk) . scrubbedCTR ek . Sized.convert
#endif

derivePublicKey :: SecretKey -> PublicKey
derivePublicKey = PublicKey . skToPk . getSecretKey
  where
    skToPk =
#ifdef ghcjs_HOST_OS
      unsafeSizedByteArray . fromRight (error "derivePublicKey: received non-hex string") . hex2bytestring . js_getPublicKey . bytestring2hex
foreign import javascript unsafe "saline.roundtripBinary(bls12_381.bls12_381.getPublicKey)($1)"
  js_getPublicKey :: Text -> Text
#else
      BLST.skToPk
#endif

newtype PublicKey = PublicKey
#if defined(ghcjs_HOST_OS)
  { unwrapPublicKey :: SizedByteArray 48 ByteString } -- Compressed
#else
  { unwrapPublicKey :: BLST.PublicKey 'BLST.G1 } -- Uncompressed
#endif
  deriving stock (Eq, Generic)
  deriving anyclass (FromJSONKey, ToJSONKey)

newtype Signature = Signature
#if defined(ghcjs_HOST_OS)
  { unwrapSignature :: SizedByteArray 96 ByteString } -- Compressed
#else
  { unwrapSignature :: BLST.Signature 'BLST.G1 'BLST.Hash } -- Uncompressed
#endif
  deriving stock (Eq, Generic)
  deriving anyclass (FromJSONKey, ToJSONKey)

type SerializeHex bs output = forall a. (a -> bs) -> (a -> output)


--TODO: upstream instances to BLST so we can use deriving
instance Show PublicKey where show = showHex compressPublicKey
instance Show Signature where show = showHex compressSignature

showHex :: SerializeHex (SizedByteArray n ByteString) String
showHex toBytes = Text.unpack . bytestring2hex . unSizedByteArray . toBytes

instance Read PublicKey where readPrec = readPrecHex decompressPublicKey
instance Read Signature where readPrec = readPrecHex decompressSignature

readPrecHex :: KnownNat n => (SizedByteArray n ByteString -> Either Text a) -> ReadPrec a
readPrecHex fromByteSized = parse . Text.pack <$> readPrec
  where
    parse hx = either (error . Text.unpack) id $ do
      fromByteSized =<< maybe (throwError "Wrong size") pure . sizedByteArray =<< hex2bytestring hx

instance Ord PublicKey where compare = compare `on` compressPublicKey
instance Ord Signature where compare = compare `on` compressSignature

instance ToJSON PublicKey where toJSON = toJSONHex compressPublicKey
instance ToJSON Signature where toJSON = toJSONHex compressSignature

toJSONHex :: SerializeHex (SizedByteArray n ByteString) Value
toJSONHex toBytes = Aeson.String . bytestring2hex . unSizedByteArray . toBytes

instance FromJSON PublicKey where parseJSON = parseJSONHex "PublicKey" decompressPublicKey
instance FromJSON Signature where parseJSON = parseJSONHex "Signature" decompressSignature

parseJSONHex :: KnownNat n => String -> (SizedByteArray n ByteString -> Either Text a) -> (Value -> Parser a)
parseJSONHex name fromBytes = Aeson.withText name $ \hx -> either (parseFail . Text.unpack) pure $ do
  bs <- hex2bytestring hx
  sbs <- note "Signature.parseJSON: wrong size" $ sizedByteArray bs
  fromBytes sbs

type Decompress n a = forall m. MonadError Text m => SizedByteArray n ByteString -> m a
#if defined(ghcjs_HOST_OS)
decompressPublicKey :: Decompress 48 PublicKey
decompressPublicKey = pure . PublicKey
decompressSignature :: Decompress 96 Signature
decompressSignature = pure . Signature
#else
decompressSignature :: Decompress (BLST.CompressedSize 'BLST.P2) Signature
decompressPublicKey :: Decompress (BLST.CompressedSize 'BLST.P1) PublicKey
decompressSignature = fmap Signature . either (throwError . tshow) pure . BLST.decompressSignature
decompressPublicKey = fmap PublicKey . either (throwError . tshow) pure . BLST.decompressPk
#endif

type Compress n a = a -> SizedByteArray n ByteString
#if defined(ghcjs_HOST_OS)
compressSignature :: Compress 96 Signature
compressPublicKey :: Compress 48 PublicKey
compressSignature = unwrapSignature
compressPublicKey = unwrapPublicKey
#else
compressSignature :: Compress (BLST.CompressedSize 'BLST.P2) Signature
compressPublicKey :: Compress (BLST.CompressedSize 'BLST.P1) PublicKey
compressSignature = convertSized . BLST.compressSignature . unwrapSignature
compressPublicKey = convertSized . BLST.compressPk . unwrapPublicKey
convertSized
  :: (KnownNat n, ByteArray.ByteArrayAccess bin, ByteArray.ByteArray bout)
  => SizedByteArray n bin -> SizedByteArray n bout
convertSized = unsafeSizedByteArray . ByteArray.convert . unSizedByteArray
#endif

prettyPrintAddress :: PublicKey -> Text
prettyPrintAddress = encode $ publicKeyEncoder @_ @(Either Text)

prettyPrintSignature :: Signature -> Text
prettyPrintSignature = encode $ signatureEncoder @_ @(Either Text)

parseAddress :: SalineAddr -> Either Text PublicKey
parseAddress = tryDecode publicKeyEncoder

parseSignature :: SalineAddr -> Either Text Signature
parseSignature = tryDecode signatureEncoder

publicKeyEncoder :: (Applicative check, MonadError Text parse) => Encoder check parse PublicKey Text
publicKeyEncoder = prefixTextEncoder "nacl:" . hexEncoder True . unSizedByteArrayEncoder
#ifdef ghcjs_HOST_OS
  . viewEncoder (iso unwrapPublicKey PublicKey)
#else
  . unsafeMkEncoder (EncoderImpl decompressPublicKey compressPublicKey)
#endif

signatureEncoder :: (Applicative check, MonadError Text parse) => Encoder check parse Signature Text
signatureEncoder = hexEncoder True
#ifdef ghcjs_HOST_OS
  . unSizedByteArrayEncoder . viewEncoder (iso unwrapSignature Signature)
#else
  . unSizedByteArrayEncoder . unsafeMkEncoder (EncoderImpl decompressSignature compressSignature)
#endif

unSizedByteArrayEncoder
  :: (Applicative check, ByteArray.ByteArrayAccess ba, KnownNat n, MonadError Text parse)
  => Encoder check parse (SizedByteArray n ba) ba
unSizedByteArrayEncoder = unsafeMkEncoder $ EncoderImpl (maybe (throwError "Wrong size") pure . sizedByteArray) unSizedByteArray

-- TODO: upstream
hexEncoder :: (Applicative check, MonadError Text parse) => Bool -> Encoder check parse ByteString Text
hexEncoder prefixedWith0x = prefixed $ unsafeMkEncoder $ EncoderImpl
  { _encoderImpl_encode = bytestring2hex
  , _encoderImpl_decode = hex2bytestring
  }
  where
    prefixed = bool id (prefixTextEncoder "0x".) prefixedWith0x

bytestrings2hex :: [ByteString] -> Text
bytestrings2hex = Text.unwords . fmap bytestring2hex

sizedBytestrings2hex :: [SizedByteArray n ByteString] -> Text
sizedBytestrings2hex = Text.unwords . fmap sizedBytestring2hex

bytestring2hex :: ByteString -> Text
bytestring2hex = extractBase16 . encodeBase16

sizedBytestring2hex :: SizedByteArray n ByteString -> Text
sizedBytestring2hex = extractBase16 . encodeBase16 . unSizedByteArray

hex2bytestring :: MonadError Text m => Text -> m ByteString
hex2bytestring txt = do
  unless (even $ Text.length txt) $
    throwError "hex2bytestring: odd length"
  let invalid = Text.filter (not . isHexDigit) txt
  unless (Text.null invalid) $
    throwError $ "hex2bytestring: found invalid chars: " <> Text.pack (nubOrd $ Text.unpack invalid) <> " in " <> txt
  pure $ decodeBase16' $ assertBase16 txt

unsafeRoundtripBinary :: String -> (Text -> Text) -> (ByteString -> ByteString)
unsafeRoundtripBinary lbl f = unsafeHex2bytestring lbl . f . bytestring2hex

unsafeHex2bytestring :: String -> Text -> ByteString
unsafeHex2bytestring lbl = fromRight (error $ lbl <> ": received non-hex string") . hex2bytestring

unsafeHex2sizedBytestring :: KnownNat n => String -> Text -> SizedByteArray n ByteString
unsafeHex2sizedBytestring lbl = fromMaybe (error $ lbl <> ": seed does not have the correct size") . sizedByteArray . unsafeHex2bytestring lbl

data KeyTree = KeyNode
  { _keyTree_keyPath :: Text
  , _keyTree_privateKey :: SecretKey
  , _keyTree_childKeys :: [KeyTree]
  }

mkTree :: SecretKey -> KeyTree
mkTree = go "m"
  where
    go path sk = KeyNode path sk $ flip fmap [0..] $ \i ->
      go (path <> "/" <> tshow i) (deriveChildEIP2333 sk i)

deriveLeaf :: BIP32Child -> KeyTree -> KeyTree
deriveLeaf index = appEndo $ foldMap (Endo . goDown) $ reverse [12381, 997, 0, 0, index]
  where
    goDown :: BIP32Child -> KeyTree -> KeyTree
    goDown i t = _keyTree_childKeys t !! fromEnum i

domain :: Maybe ByteString
domain = Just $ Text.encodeUtf8 "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_NUL_" --TODO: Nothing seems to be equivalent to Just ""

sign :: SecretKey -> ByteString -> Signature
#ifdef ghcjs_HOST_OS
sign (SecretKey secretKey) message = Signature $ unsafeSizedByteArray $ unsafeHex2bytestring "sign" $ js_sign (bytestring2hex message) (bytestring2hex secretKey)
foreign import javascript unsafe "saline.bytestring2hex(bls12_381.bls12_381.sign(saline.hex2bytestring($1), saline.hex2bytestring($2)))"
  js_sign :: Text -> Text -> Text
#else
sign (SecretKey secretKey) message = Signature $ BLST.sign secretKey message domain
#endif

aggregateSignatures :: NonEmpty Signature -> Signature
aggregateSignatures = Signature
#ifdef ghcjs_HOST_OS
  . unsafeSizedByteArray . unsafeHex2bytestring "aggregateSignatures" . js_aggregateSignatures . sizedBytestrings2hex . toList . fmap unwrapSignature
foreign import javascript unsafe "saline.bytestring2hex(bls12_381.bls12_381.aggregateSignatures(saline.hex2bytestrings($1)))"
  js_aggregateSignatures :: Text -> Text
#else
  . BLST.aggregateSignatures . fmap unwrapSignature
#endif


type Verify k = k -> ByteString -> Signature -> Maybe Text
verify :: Verify PublicKey
verify (PublicKey pubKey) msg (Signature sig) =
#ifdef ghcjs_HOST_OS
  verifyStatusFromBool $ js_verify (sizedBytestring2hex sig) (bytestring2hex msg) (sizedBytestring2hex pubKey)
foreign import javascript unsafe "bls12_381.bls12_381.verify(saline.hex2bytestring($1), saline.hex2bytestring($2), saline.hex2bytestring($3))"
  js_verify :: Text -> Text -> Text -> Bool
#else
  verifyStatus $ BLST.verify sig pubKey msg domain
#endif

aggregateVerify :: Verify (NonEmpty PublicKey)
aggregateVerify keys msg (Signature sig) =
  let
    msgKeys = flip fmap keys $ \k -> (unwrapPublicKey k, msg)
#ifdef ghcjs_HOST_OS
    (ks, msgs) = unzip $ toList msgKeys
  in
    verifyStatusFromBool $ js_verifyBatch
      (bytestring2hex $ unSizedByteArray sig)
      (bytestrings2hex msgs)
      (bytestrings2hex $ fmap unSizedByteArray $ ks)
foreign import javascript unsafe "bls12_381.bls12_381.verifyBatch(saline.hex2bytestring($1), saline.hex2bytestrings($2), saline.hex2bytestrings($3))"
  js_verifyBatch :: Text -> Text -> Text -> Bool
#else
  in
    either verifyStatus verifyStatusFromBool $ BLST.aggregateVerify msgKeys sig domain
#endif

-- TODO: normalize hsblst verification errors.
verifyStatusFromBool :: Bool -> Maybe Text
verifyStatusFromBool = bool (Just "Failed verification") Nothing

#if !defined(ghcjs_HOST_OS)
verifyStatus :: BLST.BlstError -> Maybe Text
verifyStatus = \case
  BLST.BlstSuccess -> Nothing
  err -> Just $ tshow err
#endif


addressFull :: PublicKey -> Text
addressFull = prettyPrintAddress

-- Given a text show the first and last n characters, truncating the center part with an ellipsis
-- the first n characters are taken *after* the given prefix
textEllipsis :: Text -> Text -> Int -> Text
textEllipsis txt prefix n =
  let
    nStart = n + Text.length prefix
    ellipsis = "…"
    start = Text.take nStart txt
    end = Text.takeEnd n txt
  in start <> ellipsis <> end

addressEllipsis :: PublicKey -> Text
addressEllipsis addr = textEllipsis (addressFull addr) "nacl:0x" 6

addressEllipsisBig :: PublicKey -> Text
addressEllipsisBig addr = textEllipsis (addressFull addr) "nacl:0x" 20

signatureEllipsis :: Signature -> Text
signatureEllipsis sig = textEllipsis (prettyPrintSignature sig) "0x" 6

-- TODO: move inside testing-only boundaries
unsafeGenerateTestAccount :: BIP32Child -> Text -> SecretKey
unsafeGenerateTestAccount idx mnemonic = case secretFromMnemonic <$> mnemonicFromWords (Text.words mnemonic) of
  Left err -> error $ Text.unpack $ Text.unlines
    [ "generateTestAccount: error when generating secret key: " <> err
    , "  mnemonic was " <> tshow mnemonic
    ]
  Right s -> _keyTree_privateKey $ deriveLeaf idx $ mkTree $ _secret_key s
