{-# LANGUAGE CPP #-}
module Saline.Utils.TH where

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative(liftA2)
#endif
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Language.Haskell.TH

-- When building for ghcjs, running multiple TH in one splice is much faster than separately
-- https://github.com/ghcjs/ghcjs/issues/449
-- This might change with the new js backend for ghc
deriveMany :: [a -> Q [Dec]] -> [a] -> Q [Dec]
deriveMany derivers = fmap concat . sequence . liftA2 ($) derivers

deriveStockGADT
  :: (DeriveGEQ t, DeriveGCompare t, DeriveGShow t)
  => t -> Q [Dec]
deriveStockGADT = deriveMany
  [ deriveGCompare
  , deriveGEq
  , deriveGShow
  ] . pure
