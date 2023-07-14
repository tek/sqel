{-# options_ghc -Wno-unused-foralls -fdefer-type-errors -Wno-deferred-type-errors #-}

module Sqel.Test.Error.UndeterminedParam where

import Sqel.Class.ReifySqel (ReifySqel, sqel)
import Sqel.Data.Sqel (SqelFor (SqelComp, SqelPrim))
import Sqel.Default (Sqel)
import Sqel.Dsl (Prim, Prod, Table)

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

use :: SqelFor tag table -> ()
use (SqelComp _ _ _ _ _) = ()
use (SqelPrim _ _ _) = ()

undeterminedParam ::
  ∀ (sa :: Type) .
  ()
undeterminedParam = use (table_Dat @sa @Int64)

invalidSpec :: ()
invalidSpec = use (table_Dat @Int @Int64)
