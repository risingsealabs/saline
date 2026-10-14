module Saline.Utils.Uniques where

import Control.Lens qualified as Lens
import Control.Monad (guard)
import GHC.Generics (Generic)
import qualified Data.List as List

newtype Uniques xs a = Uniques { getUniques :: xs }
  deriving stock (Eq, Ord, Show, Generic)

type Uniques' f a = Uniques (f a) a

prism :: (xs -> Maybe (Uniques xs x)) -> Lens.Prism' xs (Uniques xs x)
prism = Lens.prism' getUniques

emptyList :: Uniques' [] a
emptyList = Uniques []

list :: Eq a => [a] -> Maybe (Uniques' [] a)
list xs = do
  let xs' = List.nub xs
  guard $ length xs == length xs'
  pure $ Uniques xs'
