module Sqel.Test.Statement.DeleteTest where

import Exon (exon)
import Hedgehog (TestT, (===))
import Test (unitTest)
import Test.Tasty (TestTree, testGroup)

import Sqel.Clauses (deleteFrom, returning, where_)
import Sqel.Data.Sql (Sql, sql)
import Sqel.Data.Statement (statementSql)
import Sqel.Data.TestTables (query_Q, table_Cat)
import Sqel.Syntax.Fragments (query, table_)
import qualified Sqel.Syntax.Monad as S

statementAll :: Sql
statementAll = statementSql @_ @_ @() S.do
  f <- table_ table_Cat
  deleteFrom f.table

targetAll :: Sql
targetAll = [exon|delete from "cat"|]

test_deleteAll :: TestT IO ()
test_deleteAll = do
  targetAll === statementAll

statementCond :: Sql
statementCond = statementSql S.do
  f <- query query_Q table_Cat
  deleteFrom f.table
  where_ f.query
  returning f.table

targetCond :: Sql
targetCond =
  [sql|
  delete from "cat"
  where "nam" = $1 and ("fur")."color" = $2
  returning "num", "nam", ("fur")."color", ("fur")."density"
  |]

test_deleteCond :: TestT IO ()
test_deleteCond = do
  targetCond === statementCond

test_delete :: TestTree
test_delete =
  testGroup "delete" [
    unitTest "all" test_deleteAll,
    unitTest "condition" test_deleteCond
  ]
