{-# options_ghc -fdefer-type-errors -Wno-deferred-type-errors #-}

module Sqel.Test.Error.UndeterminedParam where

import Sqel.Build.Sql (BuildClause)
import Sqel.Class.ReifySqel (ReifySqel, sqel)
import Sqel.Data.Sqel (SqelFor)
import Sqel.Data.Statement (Statement)
import Sqel.Dd (DdType)
import Sqel.Default (From, Select, Sqel)
import Sqel.Dsl (Prim, Prod, Table)
import Sqel.Statement.Common (selectAll)

data Dat a =
  Dat {
    f1 :: Int64,
    f2 :: a
  }
  deriving stock (Eq, Show, Generic)

type Table_Dat a sa =
  Table "dat" (Dat a) (Prod [Prim, sa])

table_Dat ::
  ∀ sa a .
  ReifySqel (Table_Dat a sa) =>
  Sqel (Table_Dat a sa)
table_Dat = sqel

use ::
  BuildClause tag Select =>
  BuildClause tag From =>
  SqelFor tag table ->
  Statement () (DdType table)
use table = selectAll table

undeterminedParam :: Statement () (Dat Int64)
undeterminedParam = use (table_Dat @_ @Int64)
