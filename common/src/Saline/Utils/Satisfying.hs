{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{- import Saline.Utils.Satisfying qualified as Sat -}
module Saline.Utils.Satisfying where

import Prelude qualified
import Prelude hiding (all, div, either, even, map, negate, odd, replicate, sequence)

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative(liftA2)
#endif
import Control.Lens qualified as Lens
import Control.Monad qualified as Monad
import Data.ByteArray (ByteArray)
import Data.ByteArray qualified as ByteArray
import Data.ByteArray.Sized (SizedByteArray (unSizedByteArray), unsafeSizedByteArray)
import Data.Dependent.Sum
import Data.Foldable hiding (all)
import Data.Functor.Identity
import Data.Functor.Invariant
import Data.Proxy
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lens qualified as Lens
import Data.Traversable hiding (sequence)
import GHC.TypeNats (KnownNat, natVal)
import System.Random (Random)
import Test.QuickCheck hiding (arbitrary, choose)
import Test.QuickCheck qualified as QuickCheck

import Saline.Utils (Monoidal)
import Saline.Utils qualified as Utils

-- Reified arguments of `forAllShrink`
-- Laws/invariants
--   fmap predicate gen = pure True
--   not (predicate a) = none predicate (shrink a)
data Satisfying a = Satisfying
  { _satisfying_gen       :: Gen a
  , _satisfying_predicate :: a -> Bool
  , _satisfying_shrink    :: a -> [a]
  }

instance Invariant Satisfying where
  invmap fw bw = iso (Lens.iso fw bw)

instance Monoidal Satisfying where
  unit = Satisfying (pure ()) (\() -> True) pure
  pair sa sb = Satisfying
    { _satisfying_gen       = liftA2 (,) (_satisfying_gen sa) (_satisfying_gen sb)
    , _satisfying_predicate = \(a,b) -> _satisfying_predicate sa a && _satisfying_predicate sb b
    , _satisfying_shrink    = liftShrink2 (_satisfying_shrink sa) (_satisfying_shrink sb)
    }

-- Combinators
wildcard :: Gen a -> Satisfying a
wildcard g = Satisfying g (Prelude.const True) (const [])

arbitrary :: Arbitrary a => Satisfying a
arbitrary = wildcard QuickCheck.arbitrary

equals :: Eq a => a -> Satisfying a
equals a = Satisfying (pure a) (== a) $ const []

member :: Ord a => Set a -> Satisfying a
member s = Satisfying (QuickCheck.oneof $ fmap pure $ toList s) (`Set.member` s) $ const []

identity :: Satisfying a -> Satisfying (Identity a)
identity = iso $ Lens.iso Identity runIdentity

iso :: Lens.Iso' a b -> Satisfying a -> Satisfying b
iso i s = Lens.withIso i $ \fw bw -> Satisfying
    (fmap fw $ _satisfying_gen s)
    (_satisfying_predicate s . bw)
    (fmap fw . _satisfying_shrink s . bw)

liftIso :: Functor f => Lens.Iso' a b -> Satisfying (f a) -> Satisfying (f b)
liftIso i = iso $ Utils.liftIso i

--TODO: return Either from Prism?
prism :: Lens.Prism' b a -> Satisfying a -> Satisfying b
prism p s = Satisfying
  { _satisfying_gen       = Lens.review p <$> _satisfying_gen s
  , _satisfying_predicate = maybe False (_satisfying_predicate s) . Lens.preview p
  , _satisfying_shrink    = maybe [] (fmap (Lens.review p) . _satisfying_shrink s) . Lens.preview p
  }

swap :: Satisfying (a, b) -> Satisfying (b, a)
swap = iso Lens.swapped

positive, negative :: (Num a, Arbitrary a, Ord a) => Satisfying a
positive = Satisfying (fmap (max 1 . abs) QuickCheck.arbitrary) (> 0) shrink
negative = negate positive

negate :: Num a => Satisfying a -> Satisfying a
negate = iso $ Lens.iso Prelude.negate Prelude.negate

isoPair
  :: ( x -> Lens.Iso' a b)
  -> ( x -> Satisfying a -> Satisfying b
     , x -> Satisfying b -> Satisfying a
     )
isoPair f =
  ( \x -> iso $ f x
  , \x -> iso $ Lens.from $ f x
  )

add, sub :: Num a        => a -> Satisfying a -> Satisfying a
div, mul :: Fractional a => a -> Satisfying a -> Satisfying a
(add, sub) = isoPair $ \n -> Lens.iso (+n) (\x -> x - n)
(mul, div) = isoPair $ \n -> Lens.iso (*n) (/n)

-- TODO: `divMod` is partial when divisor is 0
multiple :: Integral a => a -> Satisfying a -> Satisfying a
multiple n = prism $ Lens.prism' (n*) $ \a -> case a `divMod` n of
  (d, 0) -> Just d
  _ -> Nothing

even, odd :: (Arbitrary a, Integral a) => Satisfying a
even = multiple 2 arbitrary
odd = add 1 even

choose :: (Random a, Ord a) => (a,a) -> Satisfying a
choose (x,y) = Satisfying
  { _satisfying_gen       = QuickCheck.choose (x,y)
  , _satisfying_predicate = \a -> x <= a && a <= y
  , _satisfying_shrink    = const []
  }

either :: Satisfying a -> Satisfying b -> Satisfying (Either a b)
either sa sb = Satisfying
  { _satisfying_gen       = oneof [ Left <$> _satisfying_gen sa, Right <$> _satisfying_gen sb ]
  , _satisfying_predicate = Prelude.either (_satisfying_predicate sa) (_satisfying_predicate sb)
  , _satisfying_shrink    = liftShrink2 (_satisfying_shrink sa) (_satisfying_shrink sb)
  }

sequence :: [Satisfying a] -> Satisfying [a]
sequence ss = Satisfying
  { _satisfying_gen       = for ss _satisfying_gen
  , _satisfying_predicate = and . zipWith _satisfying_predicate ss
  , _satisfying_shrink    = zipWith _satisfying_shrink ss
  }

all :: Satisfying a -> Satisfying [a]
all s = Satisfying
  { _satisfying_gen       = liftArbitrary (_satisfying_gen s)
  , _satisfying_predicate = Prelude.all (_satisfying_predicate s)
  , _satisfying_shrink    = liftShrink (_satisfying_shrink s)
  }

bytearray :: forall bs. ByteArray bs => Word -> Satisfying bs
bytearray n = iso (Lens.iso (ByteArray.pack @bs) ByteArray.unpack) $
  replicate (equals n) arbitrary

sizedByteArray :: forall bs n. (ByteArray bs, KnownNat n) => Satisfying (SizedByteArray n bs)
sizedByteArray = iso (Lens.iso unsafeSizedByteArray unSizedByteArray) $ bytearray (fromIntegral $ natVal $ Proxy @n)

vectorOf :: Word -> Satisfying a -> Satisfying [a]
vectorOf l = sequence . Prelude.replicate (fromEnum l)

-- TODO: use Word?
-- TODO: enforce positive length
replicate :: Satisfying Word -> Satisfying a -> Satisfying [a]
replicate sl sa = Satisfying
  { _satisfying_gen       = do
      l <- _satisfying_gen sl
      Monad.replicateM (fromEnum l) (_satisfying_gen sa)
  , _satisfying_predicate = \as -> _satisfying_predicate sl (toEnum (length as)) && Prelude.all (_satisfying_predicate sa) as
  , _satisfying_shrink    = fmap (_satisfying_shrink sa) -- TODO: change length?
  }

dsum :: (forall x. Satisfying (k x)) -> (forall x. k x -> Satisfying (v x)) -> Satisfying (DSum k v)
dsum sk f = Satisfying
  { _satisfying_gen = do
      k <- _satisfying_gen sk
      v <- _satisfying_gen (f k)
      pure $ k :=> v
  , _satisfying_predicate = \(k :=> v) -> _satisfying_predicate sk k && _satisfying_predicate (f k) v
  , _satisfying_shrink    = \(k :=> v) -> Monad.join
      [ fmap (:=> v) $ _satisfying_shrink sk k
      , fmap (k :=>) $ _satisfying_shrink (f k) v
      ]
  }

pack :: Satisfying String -> Satisfying Text
pack = iso Lens.packed

prepend :: Text -> Satisfying Text -> Satisfying Text
prepend prefix = prism $ Lens.prism' (prefix <>) $ Text.stripPrefix prefix
