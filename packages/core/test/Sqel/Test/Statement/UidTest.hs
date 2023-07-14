module Sqel.Test.Statement.UidTest where

import Hedgehog (TestT, (===))
import Prelude hiding (join, on)

import Sqel.Clauses (from, select)
import Sqel.Data.Sql (Sql, sql)
import Sqel.Data.Statement (statementSql)
import Sqel.Data.TestTables (uidTable_Simp)
import Sqel.Syntax.Fragments (table_)
import qualified Sqel.Syntax.Monad as S

statement :: Sql
statement =
  statementSql S.do
    frags <- table_ uidTable_Simp
    select frags.table.name
    from frags.table

target :: Sql
target = [sql|select "name" from "simp"|]

test_uidProjection :: TestT IO ()
test_uidProjection =
  target === statement
