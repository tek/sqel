module Sqel.Test.Migration.SyntaxTest where

import Exon (exon)
import Hasql.Session (Session)
import Hedgehog (TestT, (===))

import Sqel.Class.ReifySqel (sqel)
import Sqel.Data.Migration (Migrate)
import Sqel.Data.Sqel (SqelFor)
import Sqel.Data.Sql (Sql)
import Sqel.Default (Def)
import Sqel.Dsl (Delete, Gen, MigrationTable, Prim, Prod, Rename)
import Sqel.Migration.Class.Syntax ((-->))
import Sqel.Migration.Statement (migrationStatementSql, migrationsStatements, migrationsTableStatements)
import Sqel.Migration.Transform (transform)

data A =
  A { field1 :: Int64 }
  deriving stock (Eq, Show, Generic)

type Table_A = MigrationTable "d" A Gen

table_A :: SqelFor Def Table_A
table_A = sqel

data B =
  B {
    field1 :: Int64,
    field2 :: Int64
  }
  deriving stock (Eq, Show, Generic)

type Table_B = MigrationTable "d" B Gen

table_B :: SqelFor Def Table_B
table_B = sqel

data C =
  C {
    field1 :: Int64,
    field2 :: Int64,
    field3 :: Int64
  }
  deriving stock (Eq, Show, Generic)

type Table_C = MigrationTable "d" C (Prod [Delete Prim, Prim, Prim])

table_C :: SqelFor Def Table_C
table_C = sqel

data D =
  D {
    fieldX :: Int64,
    field3 :: Int64,
    field4 :: Int64
  }
  deriving stock (Eq, Show, Generic)

type Table_D = MigrationTable "d" D (Prod [Rename "field2" Prim, Prim, Prim])

table_D :: SqelFor Def Table_D
table_D = sqel

migrations1 :: Migrate Def Identity [Table_D, Table_C, Table_B, Table_A]
migrations1 =
  table_A --> table_B --> table_C --> table_D

migrations2 :: Migrate Def Session [Table_D, Table_C, Table_B, Table_A]
migrations2 =
  table_A --> table_B --> transform trans table_C --> table_D
  where
    trans = pure @Session . fmap \ B {..} -> C {field3 = 5, ..}

stmtsTarget :: [[Sql]]
stmtsTarget =
  [
    [
      [exon|alter table "d" drop column "field1"|],
      [exon|alter table "d" rename column "field2" to "field_x"|],
      [exon|alter table "d" add column "field4" bigint|],
      [exon|alter table "d" alter column "field4" set not null|]
    ],
    [
      [exon|alter table "d" add column "field3" bigint|],
      [exon|alter table "d" alter column "field3" set not null|]
    ],
    [
      [exon|alter table "d" add column "field2" bigint|],
      [exon|alter table "d" alter column "field2" set not null|]
    ]
  ]

tableStmtsTarget :: [[Sql]]
tableStmtsTarget =
  [
    [[exon|create table "d" ("field_x" bigint not null, "field3" bigint not null, "field4" bigint not null)|]],
    [[exon|create table "d" ("field1" bigint not null, "field2" bigint not null, "field3" bigint not null)|]],
    [[exon|create table "d" ("field1" bigint not null, "field2" bigint not null)|]],
    [[exon|create table "d" ("field1" bigint not null)|]]
  ]

test_migrationSyntax :: TestT IO ()
test_migrationSyntax = do
  stmtsTarget === (fmap migrationStatementSql <$> migrationsStatements migrations1)
  tableStmtsTarget === migrationsTableStatements migrations1
