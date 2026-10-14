{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Saline.Utils where

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative(liftA2)
#endif
import Control.Concurrent (threadDelay)
import Control.Lens (Iso', Prism', iso, prism', re, withIso, (^.), (^?), _Right)
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader (ask))
import Control.Monad.State
import Data.Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Bifunctor
import Data.Bool (bool)
import Data.Constraint.Extras
import Data.Data (type (:~:) (Refl))
import Data.Dependent.Map (DMap)
import Data.Dependent.Map qualified as DMap
import Data.Dependent.Sum
import Data.Foldable
import Data.Function ((&))
import Data.Functor.Const
import Data.Functor.Identity
import Data.Functor.Invariant (Invariant (..))
import Data.GADT.Compare (GCompare, GEq (geq))
import Data.GADT.Show
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Semigroup
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Some (Some (..), foldSome)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Time
import Data.Time.Clock.POSIX
import Data.Tuple (swap)
import GHC.Generics
import GHC.IO.Handle
import Obelisk.Route (pattern (:/), pattern (:.))
import System.Console.ANSI
import System.IO
import System.Random (genWord8)
import Test.QuickCheck.Gen (Gen (unGen))
import Test.QuickCheck.Random
import Text.Printf (printf)
import Text.Read (readEither, readMaybe)
import Text.URI (Authority (Authority), RText, RTextLabel (Host), URI, mkHost, unRText)
import Text.URI.Lens (uriAuthority)

import Saline.Utils.TH (deriveStockGADT)

-- Same as Data.Semigroup.Endo but without a newtype
type Override a = a -> a

liftIso :: Functor f => Iso' a b -> Iso' (f a) (f b) --TODO: upstream somewhere
liftIso i = withIso i $ \fw bw -> iso (fmap fw) (fmap bw)

reverseLookup :: (Ord k, Ord v) => Map k v -> Map v (Set k) --TODO: nonempty
reverseLookup = Map.fromListWith (<>) . fmap (second Set.singleton . swap) . Map.toList

curryMap :: (Ord a, Ord b) => Map (a,b) c -> Map a (Map b c)
curryMap = Map.fromListWith Map.union . fmap (second sing . associate) . Map.toList
  where
    associate ((a,b),c) = (a,(b,c))
    sing = uncurry Map.singleton

uncurryMap :: (Ord a, Ord b) => Map a (Map b c) -> Map (a,b) c
uncurryMap = Map.fromList . unroll . Map.toList . fmap Map.toList
  where
    unroll abcs = do
      (a, bcs) <- abcs
      (b,c) <- bcs
      pure ((a,b),c)

ireplicateM :: Applicative m => Word -> (Word -> m a) -> m [a]
ireplicateM n f = traverse f [0..n-1]

-- TODO: source location? HasCallStack?
impossible :: Text -> a
impossible txt = error $ Text.unpack $ "The 'impossible' has happened: " <> txt

tshow :: Show a => a -> Text
tshow = Text.pack . show

tread :: Read a => Text -> a
tread = read . Text.unpack

treadMaybe :: Read a => Text -> Maybe a
treadMaybe = readMaybe . Text.unpack

treadEither :: Read a => Text -> Either Text a
treadEither = first Text.pack . readEither . Text.unpack

argKey :: Arg k v -> k
argKey (Arg k _) = k

argValue :: Arg k v -> v
argValue (Arg _ v) = v

-- TODO: upstream to dependent-sum?
someKey :: DSum k v -> Some k
someKey (k :=> _) = Some k

someValue :: DSum k v -> Some v
someValue (_ :=> v) = Some v

mapKey :: (forall x. k x -> k' x) -> DSum k v -> DSum k' v
mapKey f (k :=> v) = f k :=> v

forKey :: Functor f => (forall x. k x -> f (k' x)) -> DSum k v -> f (DSum k' v)
forKey f (k :=> v) = fmap (:=> v) $ f k

mapValue :: (forall x. v x -> v' x) -> DSum k v -> DSum k v'
mapValue f (k :=> v) = k :=> f v

forValue :: Functor f => (forall x. v x -> f (v' x)) -> DSum k v -> f (DSum k v')
forValue f (k :=> v) = fmap (k :=>) $ f v

dsum, duncurry :: (forall x. k x -> v x -> a) -> DSum k v -> a
dsum f (k :=> v) = f k v
duncurry = dsum

dcurry :: (DSum k v -> a) -> (forall x. k x -> v x -> a)
dcurry f k v = f (k :=> v)

fromPair :: (k,v) -> DSum (Const k) (Const v)
fromPair (k,v) = Const k :=> Const v

toPair :: DSum (Const k) (Const v) -> (k,v)
toPair (Const k :=> Const v) = (k, v)

fromKey :: (forall x. k x -> v x) -> Some k -> DSum k v
fromKey f = foldSome $ \k -> k :=> f k

fromValue :: (forall x. v x -> k x) -> Some v -> DSum k v
fromValue f = swapDSum . fromKey f

swapDSum :: DSum k v -> DSum v k
swapDSum (k :=> v) = v :=> k

-- TODO: upstream to dependent-map?
compose :: (GCompare a, GCompare b) => DMap b c -> DMap a b -> DMap a c
compose bc = DMap.mapMaybe $ flip DMap.lookup bc

fromSet :: GCompare k => (forall x. k x -> v x) -> Set (Some k) -> DMap k v
fromSet f = DMap.fromList . fmap (fromKey f) . toList

route :: (forall x. f x -> x -> a) -> DSum f Identity -> a
route f (k :=> Identity v) = f k v

-- Always uses the list of pairs encoding
newtype AesonMap k v = AesonMap (Map k v)
  deriving stock Generic
  deriving newtype (Eq, Ord, Show)

instance (FromJSON k, FromJSON v, Ord k) => FromJSON (AesonMap k v) where
  parseJSON = fmap (AesonMap . Map.mapKeysMonotonic (\(NoJSONKey k) -> k)) . parseJSON
instance (ToJSON k, ToJSON v, Ord k) => ToJSON (AesonMap k v) where
  toJSON = toJSON . Map.mapKeysMonotonic NoJSONKey . (\(AesonMap m) -> m)

newtype NoJSONKey k = NoJSONKey k
  deriving stock Generic
  deriving newtype (Eq, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

instance ToJSON k => ToJSONKey (NoJSONKey k) where
  toJSONKey = Aeson.ToJSONKeyValue Aeson.toJSON Aeson.toEncoding
instance FromJSON k => FromJSONKey (NoJSONKey k) where
  fromJSONKey = Aeson.FromJSONKeyValue Aeson.parseJSON
{-
  We often want to use types of kind (* -> *) directly as type
    parameters as well as applying them to use as key of a DSum.

  One might try defining
    `type Foo a = DSum (k a) v`
  but when trying to use as
    `Bar Foo`
  we get an error that `Foo` is lacking arguments as we need to supply `a`

  Until GHC allows unsaturated type families, one needs to use newtypes
    with `a` in the last position so it can be used partially applied.

  This definition allows us to have only one such newtype and then do
    `type Foo = NewtypeDSum k v`
-}
newtype NewtypeDSum k v a = NewtypeDSum { unNewtypeDSum :: DSum (k a) v }
  deriving stock Generic
deriving newtype  instance (Has' Eq       (k a) v, GEq      (k a)                 ) => Eq   (NewtypeDSum k v a)
deriving newtype  instance (Has' Ord      (k a) v, GCompare (k a), Has' Eq (k a) v) => Ord  (NewtypeDSum k v a)
deriving newtype  instance (Has' Show     (k a) v, GShow    (k a)                 ) => Show (NewtypeDSum k v a)

--TODO: upstream
instance ToJSON   (DSum k v) => ToJSONKey   (DSum k v)
instance FromJSON (DSum k v) => FromJSONKey (DSum k v)

instance ToJSON (DSum k v) => ToJSON (DMap k v) where
    toJSON = toJSON . DMap.toList
instance (GCompare k, FromJSON (DSum k v)) => FromJSON (DMap k v) where
    parseJSON = fmap DMap.fromList . parseJSON

newtypeDSum :: (forall x. k a x -> v x -> b) -> NewtypeDSum k v a -> b
newtypeDSum f = dsum f . unNewtypeDSum
{-
  Taken from https://github.com/obsidiansystems/obelisk/blob/135bfd7422b856c146b5cd5dacfe584f0621825e/lib/route/src/Obelisk/Route.hs#L859-L886
  TODO: upstream to dependent-sum or so
-}
dSumPrism
  :: forall f f' g
  .  (forall a. Prism' (f a) (f' a))
  -> Prism' (DSum f g) (DSum f' g)
dSumPrism p = prism'
  (\(f' :=> x) -> f' ^. re p :=> x)
  (\(f :=> x) -> (:=> x) <$> (f ^? p))

dSumPrism'
  :: forall f g a
  .  (forall b. Prism' (f b) (a :~: b))
  -> Prism' (DSum f g) (g a)
dSumPrism' p = dSumPrism p . iso (\(Refl :=> b) -> b) (Refl :=>)

dSumGEqPrism
  :: GEq f
  => f a
  -> Prism' (DSum f g) (g a)
dSumGEqPrism variant = dSumPrism' $ prism' (\Refl -> variant) (geq variant)

dSumGEqIdentityPrism
  :: GEq f
  => f a
  -> Prism' (DSum f Identity) a
dSumGEqIdentityPrism variant = dSumGEqPrism variant . iso runIdentity Identity

newtypeDSumGEqIdentityPrism
  :: GEq (k a)
  => k a x
  -> Prism' (NewtypeDSum k Identity a) x
newtypeDSumGEqIdentityPrism k = iso unNewtypeDSum NewtypeDSum . dSumGEqIdentityPrism k

-- TODO: check whether QuickCheck exposes a suitable stateful generator runner.
generate :: Monad m => Gen a -> StateT QCGen m a
generate g = state $ \old ->
  let (_, new) = genWord8 old -- Advance the generator state before sampling.
  in (unGen g new size, new)
  where
   size = 30 -- default used in Test.QuickCheck.Gen.generate

-- TODO: refined?
nonEmptyText :: Text -> Maybe Text
nonEmptyText = \case
  "" -> Nothing
  t -> Just t

sleep :: Word -> IO ()
sleep = threadDelay . fromEnum . seconds
  where seconds = (*1e6)

initProcess :: MonadIO m => m ()
initProcess = liftIO $ do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

data ThreadEnv = ThreadEnv
  { _threadEnv_component :: Text
  , _threadEnv_color     :: Color
  }

logger :: (MonadIO m, MonadReader ThreadEnv m) => Text -> m ()
logger txt = do
  ThreadEnv name color <- ask
  liftIO $ Text.putStrLn $ sgrify [SetColor Foreground Dull color] $ name <> " \t" <> txt

sgrify :: [SGR] -> Text -> Text
sgrify codes txt = mconcat
  [ Text.pack $ setSGRCode codes
  , txt
  , Text.pack $ setSGRCode [Reset]
  ]

localhost :: Text
localhost = "127.0.0.1"

localhostRText :: RText 'Host
localhostRText = fromMaybe (error "localhostRText: invalid IP") $ mkHost localhost

everyInterface :: Text
everyInterface = "0.0.0.0"

everyInterfaceRText :: RText 'Host
everyInterfaceRText = fromMaybe (error "everyInterfaceRText: invalid IP") $ mkHost everyInterface

unsafeHostPortFromURI :: URI -> (Text, Word)
unsafeHostPortFromURI uri = (h,p)
  where
    err component = error $ "unsafeHostPortFromURI: URI did not contain " <> component
    Authority _ host port = fromMaybe (err "Authority") $ uri ^. uriAuthority ^? _Right
    p = fromMaybe (err "Port") port
    h = unRText host

cleave :: Text -> Text -> (Text, Text)
cleave sep str =
  let (a,b) = Text.breakOn sep str
  in (a, Text.drop (Text.length sep) b)

-- | Pretty print a fixed number of decimals on a USD amount
-- For now, since this is just for display, we display "~0.00xx" when the amount is less than
-- the decimal
prettyPrintWithDecimals :: Bool -> Int -> Double -> Text
prettyPrintWithDecimals showDollarSign nbDec val  =
  let
    approximate = 0 < val && val < 1 / 10^nbDec
    prefixIf cond symbol = bool id (symbol <>) cond
  in
    printf ("%." ++ show nbDec ++ "f") val
      & Text.pack
      & Text.dropWhileEnd (== '0')
      & Text.dropWhileEnd (== '.')
      & prefixIf showDollarSign "$"
      & prefixIf approximate "~"

unixEpoch :: UTCTime
unixEpoch = UTCTime (fromGregorian 1970 0 0) 0

timestamp :: POSIXTime -> Text
timestamp d = Text.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" $ posixSecondsToUTCTime d

-- Add an 's' for plural on a noun, based on the given number
simplePlural :: (Show a, Eq a, Num a) => a -> Text -> Text
simplePlural n txt = tshow n <> " " <> bool (<> "s") id (n == 1) txt

wrapped :: Text -> Text -> Text -> Text
wrapped start end x = start <> x <> end

wrappedSym :: Text -> Text -> Text
wrappedSym both = wrapped both both

brackets, curly, parens, doubleQuotes, singleQuotes :: Text -> Text
singleQuotes = wrappedSym "'"
doubleQuotes = wrappedSym "\""
parens       = wrapped "(" ")"
brackets     = wrapped "[" "]"
curly        = wrapped "{" "}"

noPrefix :: String -> String -> String
noPrefix p str = fromMaybe str $ List.stripPrefix p str


-- TODO: really shouldn't be reinventing wheels where but
-- categories/contravariant/invariant/invertible packages aren't general enough
-- and hask/monoidal-functors packages are super general
isomap :: Invariant f => Iso' a b -> f a -> f b
isomap = flip withIso invmap

class Invariant f => Monoidal f where
  unit :: f ()
  pair :: f x -> f y -> f (x,y)

instance Monoid a => Monoidal (Const a) where
  unit = pure ()
  pair = liftA2 (,)

triple :: Monoidal f => f a -> f b -> f c -> f (a,b,c)
triple a b = invmap fw bw . pair a . pair b
  where
    bw (x, y, z) = x :. y :. z
    fw (x :. y :. z) = (x, y, z)

tuple4 :: Monoidal f => f a -> f b -> f c -> f d -> f (a,b,c,d)
tuple4 a b c = invmap fw bw . triple a b . pair c
  where
    fw (w,x,(y,z)) = (w,x,y,z)
    bw (w,x,y,z) = (w,x,(y,z))

-- TODO: upstream to
-- import "patch" Data.Functor.Misc
data MaybeTag a x where
  JustTag :: MaybeTag a a
  NothingTag :: MaybeTag a ()

dsumToMaybe :: DSum (MaybeTag a) Identity -> Maybe a
dsumToMaybe = \case
  NothingTag :/ () -> Nothing
  JustTag :/ a -> Just a

maybeToDSum :: Maybe a -> DSum (MaybeTag a) Identity
maybeToDSum = \case
      Nothing -> NothingTag :/ ()
      Just a -> JustTag :/ a

deriveStockGADT ''MaybeTag
