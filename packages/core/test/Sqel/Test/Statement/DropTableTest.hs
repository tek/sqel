module Sqel.Test.Statement.DropTableTest where

import Hedgehog (TestT, (===))
import Test (unitTest)
import Test.Tasty (TestTree, testGroup)

import Sqel.Clauses (dropTable)
import Sqel.Data.Drop (Cascade (Cascade), Drop (Drop))
import Sqel.Data.Sql (Sql, sql)
import Sqel.Data.Statement (statementSql)
import Sqel.Syntax.Fragments (table)
import qualified Sqel.Syntax.Monad as S
import Sqel.Data.TestTables (table_Cat)

statement1 :: Sql
statement1 = statementSql @_ @() S.do
  t <- table table_Cat
  dropTable t (Drop False Nothing)

statement2 :: Sql
statement2 = statementSql @_ @() S.do
  t <- table table_Cat
  dropTable t (Drop True Nothing)

statement3 :: Sql
statement3 = statementSql @_ @() S.do
  t <- table table_Cat
  dropTable t (Drop True (Just Cascade))

target1 :: Sql
target1 = [sql|drop table "cat"|]

target2 :: Sql
target2 = [sql|drop table if exists "cat"|]

target3 :: Sql
target3 = [sql|drop table if exists "cat" cascade|]

test_dropTable1 :: TestT IO ()
test_dropTable1 =
  target1 === statement1

test_dropTable2 :: TestT IO ()
test_dropTable2 =
  target2 === statement2

test_dropTable3 :: TestT IO ()
test_dropTable3 =
  target3 === statement3

test_dropTable :: TestTree
test_dropTable =
  testGroup "drop table" [
    unitTest "basic" test_dropTable1,
    unitTest "if exists" test_dropTable2,
    unitTest "cascade" test_dropTable3
  ]
