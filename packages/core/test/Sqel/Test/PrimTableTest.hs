module Sqel.Test.PrimTableTest where

import Exon (exon)
import Hedgehog (TestT, (===))

import Sqel.Class.ReifySqel (sqel)
import Sqel.Clauses (from, select)
import Sqel.Data.Sql (Sql)
import Sqel.Data.Statement (statementSql)
import Sqel.Default (Sqel)
import Sqel.Dsl (Prim, Table)
import Sqel.Syntax.Fragments (table)
import qualified Sqel.Syntax.Monad as S

type Table_Name = Table "name" Text Prim

table_Name :: Sqel Table_Name
table_Name = sqel

statement :: Sql
statement =
  statementSql S.do
    t <- table table_Name
    select t
    from t

target :: Sql
target = [exon|select "name" from "name"|]

test_primTable :: TestT IO ()
test_primTable =
  target === statement
