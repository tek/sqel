module Sqel.Test.Statement.JoinTest where

import Hedgehog (TestT, (===))
import Prelude hiding (join, on)

import Sqel.Clauses (from, join, on, select, where_)
import Sqel.Data.Sql (Sql, sql)
import Sqel.Data.Statement (statementSql)
import Sqel.Data.TestTables (query_Q, table_Bird, table_Cat)
import Sqel.Fragment ((.=))
import Sqel.Syntax.Fragments (query)
import qualified Sqel.Syntax.Monad as S

statement :: Sql
statement =
  statementSql S.do
    frags <- query query_Q (table_Cat, table_Bird)
    select frags.cat
    from frags.cat
    join frags.bird
    on (frags.cat.nam .= frags.bird.cat)
    where_ frags.query

target :: Sql
target = [sql|
  select "cat"."num", "cat"."nam", ("cat"."fur")."color", ("cat"."fur")."density"
  from "cat" cat
  join "bird" bird
  on "cat"."nam" = "bird"."cat"
  where "cat"."nam" = $1 and ("cat"."fur")."color" = $2
  |]

test_join :: TestT IO ()
test_join =
  target === statement
