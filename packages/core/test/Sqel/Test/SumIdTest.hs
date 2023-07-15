module Sqel.Test.SumIdTest where

import Hedgehog (TestT, (===))

import Sqel.Class.ReifySqel (sqel)
import Sqel.Data.Sql (Sql, sql)
import Sqel.Data.Statement (statementSql)
import Sqel.Data.TestTables (SI, Simp, Type_SI)
import Sqel.Default (Sqel)
import Sqel.Dsl (Gen, UidTable)
import qualified Sqel.Statement.Common as Statement

type Table_SI = UidTable "simp" SI Simp Type_SI Gen

table_SI :: Sqel Table_SI
table_SI = sqel

target_create :: Sql
target_create =
  [sql|
  create table "simp"
  ("id" "sqel_type__si" primary key not null,
  "name" text not null,
  "number" bigint not null)
  |]

target_insert :: Sql
target_insert =
  [sql|
  insert into "simp" ("id", "name", "number")
  values (row($1, $2, $3), $4, $5)
  |]

test_sumId :: TestT IO ()
test_sumId = do
  target_create === statementSql (Statement.createTable table_SI)
  target_insert === statementSql (Statement.insert table_SI)
