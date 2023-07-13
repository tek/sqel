module Sqel.Test.Statement.SumQueryTest where

import Hedgehog (TestT, (===))

import Sqel.Class.ReifySqel (sqel)
import Sqel.Clauses (from, select, where_)
import Sqel.Data.Sql (Sql, sql)
import Sqel.Data.Statement (statementSql)
import Sqel.Default (Sqel)
import Sqel.Dsl (Gen, Table)
import Sqel.Syntax.Fragments (query)
import qualified Sqel.Syntax.Monad as S
import Sqel.Data.TestTables (query_NaNu)

data Dat =
  Dat {
    name :: Text,
    number :: Int64
  }
  deriving stock (Eq, Show, Generic)

type Table_Dat = Table "dat" Dat Gen

table_Dat :: Sqel Table_Dat
table_Dat = sqel

statement :: Sql
statement =
  statementSql S.do
    frags <- query query_NaNu table_Dat
    select frags.table
    from frags.table
    where_ frags.query

target :: Sql
target = [sql|select "name", "number" from "dat" where ($1 = 0 and "name" = $2) or ($1 = 1 and "number" = $3)|]

test_sumQuery :: TestT IO ()
test_sumQuery =
  target === statement
