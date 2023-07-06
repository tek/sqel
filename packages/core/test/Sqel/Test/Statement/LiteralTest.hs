module Sqel.Test.Statement.LiteralTest where

import Hedgehog (TestT, (===))

import Sqel.Clauses (from, limit, select, where_)
import Sqel.Data.Sql (Sql, sql)
import Sqel.Data.Statement (statementSql)
import Sqel.Data.TestTables (table_Simp)
import Sqel.Fragment (lit, (.=))
import Sqel.Syntax.Fragments (table_)
import qualified Sqel.Syntax.Monad as S

target_literal :: Sql
target_literal =
  [sql|select "name", "number" from "simp" where "name" = "name" and "name" = 'thing' and "number" = 5 limit 1|]

test_statement_literal :: TestT IO ()
test_statement_literal =
  target_literal === statementSql S.do
    frags <- table_ table_Simp
    select frags.table
    from frags.table
    where_ (frags.table.name .= frags.table.name, frags.table.name .= ("thing" :: Text), frags.table.number .= (5 :: Int64))
    limit (lit (1 :: Int64))
