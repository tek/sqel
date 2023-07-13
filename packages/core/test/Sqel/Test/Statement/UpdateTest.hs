module Sqel.Test.Statement.UpdateTest where

import Hedgehog (TestT, (===))

import Sqel.Class.ReifySqel (sqel)
import Sqel.Clauses (set, update, where_)
import Sqel.Data.Sql (Sql, sql)
import Sqel.Data.Statement (statementSql)
import Sqel.Data.TestTables (table_Cat)
import Sqel.Default (Sqel)
import Sqel.Dsl (Param, Prim, Prod, Query)
import Sqel.Fragment ((.=))
import Sqel.Syntax.Fragments (query)
import qualified Sqel.Syntax.Monad as S

data UQ =
  UQ {
    nam :: Text,
    num :: Int64,
    newName :: Text
  }
  deriving stock (Eq, Show, Generic)

type Query_UQ = Query UQ (Prod [Prim, Prim, Param Prim])

query_UQ :: Sqel Query_UQ
query_UQ = sqel

statement :: Sql
statement =
  statementSql @_ @_ @() S.do
    frags <- query query_UQ table_Cat
    update frags.table
    set (frags.table.nam .= frags.query.newName)
    where_ frags.query

target :: Sql
target = [sql|
  update "cat"
  set "nam" = $3
  where "nam" = $1 and "num" = $2
  |]

test_update :: TestT IO ()
test_update =
  target === statement
