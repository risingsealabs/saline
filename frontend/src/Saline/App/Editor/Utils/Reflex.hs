module Saline.App.Editor.Utils.Reflex where

import Control.Lens (set, _1, _2)
import Control.Monad
import Control.Monad.Fix
import Reflex.Dom.Core hiding (tag)
import Obelisk.Route.Frontend

runWithReplace_ :: Adjustable t m => m () -> Event t (m ()) -> m ()
runWithReplace_ a = void . runWithReplace a

--   Some reflex-based libs only expose constructs (e.g. prerender, askRoute) that expose a `Dynamic t`
-- which makes it painful to supply initial values (for instance, based on initial route value).
--
--   For prerender in particular it doesn't matter that much since hydration only kicks in once and
-- builds a new DOM anyway so we can just use `dyn`. Sometimes that's also good enough for `askRoute´
-- but problems start when we want the page/widget to also update the route (e.g. based on form state).
-- In which case to avoid re-creating the thing with the value it just gave us we either
-- use `holdUniqDyn` (see `dynRouteUniq`) or go full-on abstraction inversion by sampling with
-- getPostBuild which in turn introduces a superflous Event layer that doesn't
-- actually signify a delay as the post build event triggers 'immediately'.
-- This util captures patterns using that last workaround.
--
-- TODO: avoid this mess in the first place by making the libs return `(a, Event t a)` or equivalent instead
-- See also https://github.com/reflex-frp/reflex-dom/pull/447
unHoldDyn :: PostBuild t m => Dynamic t a -> m (Event t (a, Event t a))
unHoldDyn d = do
  pb <- getPostBuild
  pure $ ffor (current d <@ pb) (, updated d)

dynRoute_ :: (Adjustable t m, NotReady t m, PostBuild t m, Routed t r m) => (r -> m ()) -> m ()
dynRoute_ = void . dynRoute

dynRoute :: (Adjustable t m, NotReady t m, PostBuild t m, Routed t r m) => (r -> m a) -> m (Event t a)
dynRoute f = dyn . fmap f =<< askRoute

dynRouteUniq_ :: (Adjustable t m, Eq r, MonadFix m, MonadHold t m, NotReady t m, PostBuild t m, Routed t r m) => (r -> m ()) -> m ()
dynRouteUniq_ = void . dynRouteUniq

-- Sometimes even `holdUniqDyn` isn't enough. See `unHoldDyn`
dynRouteUniq :: (Adjustable t m, Eq r, MonadFix m, MonadHold t m, NotReady t m, PostBuild t m, Routed t r m) => (r -> m a) -> m (Event t a)
dynRouteUniq f = dyn . fmap f =<< holdUniqDyn =<< askRoute

data Hold t a = Hold { unHold :: forall m. (Adjustable t m, MonadFix m, MonadHold t m) => m (a, Event t a) }
deriving stock instance Functor (Hold t)

instance Applicative (Hold t) where
  pure a = Hold $ pure (a, never)

  fh <*> xh = Hold $ do
    (f, fEv) <- unHold fh
    (x, xEv) <- unHold xh
    h <- foldDyn ($) (f,x) $ leftmost
      [ ffor fEv $ set _1
      , ffor xEv $ set _2
      ]
    pure (f x, fmap (uncurry ($)) (updated h))

mkHold :: a -> Event t a -> Hold t a
mkHold a ev = Hold $ pure (a,ev)

withHold
  :: (Adjustable t m, MonadFix m, MonadHold t m)
  => Hold t a -> (a -> Event t a -> b) -> m b
withHold h f = uncurry f <$> unHold h

withHoldM
  :: (Adjustable t m, MonadFix m, MonadHold t m)
  => Hold t a -> (a -> Event t a -> m b) -> m b
withHoldM h = join . withHold h

runHold_
  :: (Adjustable t m, MonadFix m, MonadHold t m)
  => Hold t (m a) -> m ()
runHold_ = void . runHold

runHold
  :: (Adjustable t m, MonadFix m, MonadHold t m)
  => Hold t (m a) -> m (Hold t a)
runHold h = ffor (withHoldM h runWithReplace) $ \x -> Hold $ pure x

flattenHold :: Reflex t => Hold t (Hold t a) -> Hold t a
flattenHold hh = Hold $ do
  (h0, hEv) <- unHold hh
  (a0, aEv0) <- unHold h0

  ((), ev) <- runWithReplace (pure ()) $ ffor hEv unHold

  activeEv <- switchHold aEv0 $ ffor ev snd
  let switchEv = ffor ev fst

  pure (a0, leftmost [switchEv, activeEv])
