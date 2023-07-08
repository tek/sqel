module Sqel.Test.Statement.UpsertTest where

import Hedgehog (TestT, (===))
import Test (unitTest)
import Test.Tasty (TestTree, testGroup)

import Sqel.Clauses (doUpdateSet, insertInto, onConflict, values)
import Sqel.Data.Sql (Sql, sql)
import Sqel.Data.Statement (statementSql)
import Sqel.Data.TestTables (table_Cat)
import Sqel.Syntax.Fragments (table)
import qualified Sqel.Syntax.Monad as S

statement :: Sql
statement =
  statementSql @_ @_ @() S.do
    t <- table table_Cat
    insertInto t
    values t
    onConflict t
    doUpdateSet t

target :: Sql
target = [sql|
  insert into "cat" ("num", "nam", "fur")
  values ($1, $2, row($3, $4))
  on conflict ("nam")
  do update set "num" = $1, "nam" = $2, "fur" = row($3, $4)
  |]

test_upsert1 :: TestT IO ()
test_upsert1 =
  target === statement

test_upsert :: TestTree
test_upsert =
  testGroup "upsert" [
    unitTest "basic" test_upsert1
  ]
