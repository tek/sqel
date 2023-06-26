module Sqel.Test.Statement.EmptyQueryTest where

import Hedgehog (TestT, (===))

import Sqel.Clauses (from, select, where_)
import Sqel.Data.Sql (Sql, sql)
import Sqel.Sqel (emptyQuery)
import Sqel.Syntax.Fragments (query1)
import qualified Sqel.Syntax.Monad as S
import Sqel.Test.Statement.Common (table_Cat)

statement :: Sql
statement = S.do
  frags <- query1 emptyQuery table_Cat
  select frags.table
  from frags.table
  where_ frags.query

target :: Sql
target = [sql|select "num", "nam", ("fur")."color", ("fur")."density" from "cat"|]

test_statement_emptyQuery :: TestT IO ()
test_statement_emptyQuery = target === statement
