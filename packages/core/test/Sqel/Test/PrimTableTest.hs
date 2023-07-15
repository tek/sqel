module Sqel.Test.PrimTableTest where

import Hedgehog (TestT, (===))

import Sqel.Class.ReifySqel (sqel)
import Sqel.Data.Sql (Sql, sql)
import Sqel.Data.Statement (statementSql)
import Sqel.Default (Sqel)
import Sqel.Dsl (PrimAs, Table)
import qualified Sqel.Statement.Common as Statement

type Table_Name = Table "tab" Text (PrimAs "col")

table_Name :: Sqel Table_Name
table_Name = sqel

targetSelect :: Sql
targetSelect = [sql|select "col" from "tab"|]

targetCreate :: Sql
targetCreate = [sql|create table "tab" ("col" text not null)|]

test_primTable :: TestT IO ()
test_primTable = do
  targetSelect === statementSql (Statement.selectAll table_Name)
  targetCreate === statementSql (Statement.createTable table_Name)
