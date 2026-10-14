{-# LANGUAGE TypeFamilies #-}
module Saline.Core.Classes where

import Data.Kind (Type)

class Era e where
    data Block e :: Type
    data Tx e :: Type
    data Intent e :: Type
    data Ledger e :: Type
    data Addr e :: Type
    -- ...

--    validateTx :: Tx e -> Intent e -> Ledger e -> Either (Err e) (Ledger e)
    -- ...
