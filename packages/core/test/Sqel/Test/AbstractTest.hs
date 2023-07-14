module Sqel.Test.AbstractTest where

import Exon (exon)
import Hedgehog (TestT, (===))

import Sqel.Class.Check (Check1)
import Sqel.Class.ReifySqel (ReifySqel, sqel)
import Sqel.Clauses (from, select, where_)
import Sqel.Data.Sql (Sql)
import qualified Sqel.Data.Statement as Statement
import Sqel.Data.Statement (Statement)
import Sqel.Default (Sqel)
import Sqel.Dsl (Gen, Prim, Prod, Query, Table)
import Sqel.Syntax.Fragments (query, table_)
import qualified Sqel.Syntax.Monad as S

data Dat a =
  Dat {
    f1 :: a,
    f2 :: Int64
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
target = [exon|select "f1", "f2" from "dat"|]

test_abstract :: TestT IO ()
test_abstract =
  target === (statement @Prim @Text).sql

data Q =
  Q {
    f2 :: Int64
  }
  deriving stock (Eq, Show, Generic)

type Query_Q = Query Q Gen

query_Q :: Sqel Query_Q
query_Q = sqel

statementQ ::
  ∀ (sa :: Type) a .
  Check1 (Table_Dat a sa) Query_Q =>
  ReifySqel (Table_Dat a sa) =>
  Statement '[Dat a] Q (Dat a)
statementQ = S.do
  frags <- query query_Q tab
  select frags.table
  from frags.table
  where_ frags.query
  where
    tab = table_Dat @sa @a

targetQ :: Sql
targetQ = [exon|select "f1", "f2" from "dat" where "f2" = $1|]

test_abstract_query :: TestT IO ()
test_abstract_query =
  targetQ === (statementQ @Prim @Text).sql
