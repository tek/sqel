module Sqel.Test.Statement.UpdateTest where

import Hedgehog (TestT, (===))

import Sqel.Clauses (from, set, update, where_)
import Sqel.Data.Sql (Sql, sql)
import Sqel.Data.Statement (statementSql)
import Sqel.Data.TestTables (query_UQ, table_Bird, table_Cat)
import Sqel.Fragment ((.=))
import Sqel.Syntax.Fragments (query)
import qualified Sqel.Syntax.Monad as S

statement :: Sql
statement =
  statementSql @() S.do
    frags <- query query_UQ (table_Cat, table_Bird)
    update frags.cat
    set (frags.cat.nam .= frags.query.newName)
    from frags.bird
    where_ (frags.query, frags.bird.cat .= frags.cat.nam)

target :: Sql
target = [sql|
  update "cat" cat
  set "nam" = $3
  from "bird" bird
  where "bird"."cat" = $1 and "cat"."num" = $2 and "bird"."cat" = "cat"."nam"
  |]

test_update :: TestT IO ()
test_update =
  target === statement
