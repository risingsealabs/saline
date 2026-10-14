module Saline.App.Editor.Utils.Editor (Editor, Editor', iso, pair, triple, pointwise, these, unit, newtypeDSum) where

import Control.Applicative
import Control.Lens (Iso', from, view, _1, _2, _3)
import Control.Lens qualified as Lens
import Data.Dependent.Sum
import Data.Distributive
import Data.These (These)
import Data.These.Combinators

import Saline.App.Editor.Utils.Reflex
import Saline.Utils (NewtypeDSum(..))
import Saline.Utils.Field (Field)
import Saline.Utils.Field qualified as Field

type Out t e x = Hold t (Field e x)

-- TODO: worth making into a separate type? could hide t/m as existentials?
type Editor t m e a b = Maybe a -> m (Hold t (Field e b))
type Editor' t m e a = Editor t m e a a

iso :: Functor m => Iso' a b -> Editor' t m e a -> Editor' t m e b
iso i f = (fmap . fmap . fmap) (view i) . f . fmap (view $ from i)

unit :: Applicative m => Editor t m e () ()
unit = const $ pure $ pure $ pure ()

pair
  :: ( Functor m
     , outA ~ Out t e a'
     , outB ~ Out t e b'
     )
  => Editor t m e a a'
  -> Editor t m e b b'
  -> (m outA -> m outB -> m (outA, outB))
  -> Editor t m e (a,b) (a', b')
pair ea eb f input = uncurry ((liftA2 . liftA2) (,)) <$> f
  (ea $ fst <$> input)
  (eb $ snd <$> input)

triple
  :: ( Functor m
     , outA ~ Out t e a'
     , outB ~ Out t e b'
     , outC ~ Out t e c'
     )
  => Editor t m e a a'
  -> Editor t m e b b'
  -> Editor t m e c c'
  -> (m outA -> m outB -> m outC -> m (outA, outB, outC))
  -> Editor t m e (a,b,c) (a', b', c')
triple ea eb ec f input = flip fmap m $ \(a,b,c) -> (liftA3 . liftA3) (,,) a b c
  where
    m = f
      (ea $ view _1 <$> input)
      (eb $ view _2 <$> input)
      (ec $ view _3 <$> input)

pointwise
  :: ( Functor m, Distributive f, Traversable f
     , out ~ Out t e a'
     )
  => Editor t m e a a'
  -> (f (m out) -> m (f out))
  -> Editor t m e (f a) (f a')
pointwise editor f = fmap (fmap sequenceA . sequenceA) . f . fmap editor . distribute

these
  :: ( Functor m
     , outA ~ Out t e a'
     , outB ~ Out t e b'
     )
  => Editor t m e a a'
  -> Editor t m e b b'
  -> (m outA -> m outB -> m (outA, outB))
  -> Editor t m e (These a b) (These a' b')
these ea eb f input = uncurry (liftA2 Field.these) <$> f
  (ea $ justHere =<< input)
  (eb $ justThere =<< input)

newtypeDSum
  :: Functor m
  => Editor' t m e (DSum (k a) v)
  -> Editor' t m e (NewtypeDSum k v a)
newtypeDSum = iso $ Lens.iso NewtypeDSum unNewtypeDSum
