{-# LANGUAGE UndecidableInstances #-}
module Saline.Intents where

import Data.Map (Map)

import Saline.Intents.Condition

type Wallet = (Map Token Amount, Maybe Intent)
type Balances = Map Token Amount

type Intent = Condition Expr Expr []
type Contracts a = Map Intent a -- TODO: should probably be `(a Word)` and non-zero Word

or :: Condition e p [] -> Condition e p [] -> Condition e p []
or i = Condition_Any 1 . (i :) . \case
  Condition_Any 1 cs -> cs
  c -> [c]
