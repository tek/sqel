{-# options_ghc -fdefer-type-errors -Wno-deferred-type-errors #-}

module Sqel.Test.Error.CountMismatch where

import Sqel.Class.ReifySqel (sqel)
import Sqel.Data.Sqel (SqelFor (SqelComp))
import Sqel.Default (Sqel)
import Sqel.Dsl (Prim, Prod, Table)

data Dat =
  Dat {
    f1 :: Int64,
    f2 :: Int64,
    f3 :: Int64
  }
  deriving stock (Eq, Show, Generic)

type Table_More =
  Table "dat" Dat (Prod [Prim, Prim, Prim, Prim])

table_Dat :: Sqel Table_More
table_Dat = sqel

countMismatch :: ()
countMismatch = case table_Dat of
  SqelComp _ _ _ _ _ -> ()
