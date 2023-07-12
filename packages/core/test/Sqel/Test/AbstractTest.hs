module Sqel.Test.AbstractTest where

import Exon (exon)
import Hedgehog (TestT, (===))

import Sqel.Class.ReifySqel (ReifySqel, sqel)
import Sqel.Clauses (from, select)
import Sqel.Data.Sql (Sql)
import qualified Sqel.Data.Statement as Statement
import Sqel.Data.Statement (Statement)
import Sqel.Default (Sqel)
import Sqel.Dsl (Prim, Prod, Table)
import Sqel.Syntax.Fragments (table_)
import qualified Sqel.Syntax.Monad as S

data Dat a =
  Dat {
    id :: Int64,
    f2 :: a
  }
  deriving stock (Eq, Show, Generic)

type Table_Dat a sa = Table "dat" (Dat a) (Prod [Prim, sa])

table_Dat ::
  ∀ sa a .
  ReifySqel (Table_Dat a sa) =>
  Sqel (Table_Dat a sa)
table_Dat = sqel

statement ::
  ∀ (sa :: Type) a .
  ReifySqel (Table_Dat a sa) =>
  Statement '[Dat a] () (Dat a)
statement = S.do
  frags <- table_ tab
  select frags.table
  from frags.table
  where
    tab = table_Dat @sa @a

target :: Sql
target = [exon|select "id", "f2" from "dat"|]

test_abstract :: TestT IO ()
test_abstract = do
  target === (statement @Prim @Text).sql

-- type UidTable_Simp i = UidTable "simp" i Simp Prim Gen

-- uidtable_Simp ::
--   ∀ i .
--   Sqel (UidTable_Simp i)
-- uidtable_Simp = sqel
