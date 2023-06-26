module Sqel.Test.Migration.SumTest where

import Exon (exon)
import Hedgehog (TestT, (===))

import Sqel.Class.ReifySqel (sqel)
import Sqel.Data.Migration (Migrate)
import Sqel.Data.Sqel (SqelFor)
import Sqel.Data.Sql (Sql)
import Sqel.Default (Def)
import Sqel.Dsl (Con, Gen, IndexPrefix, Merge, MigrationTable, Name, Prim, Prod, RenameIndex, Sum, Table)
import Sqel.Migration.Class.Syntax ((-->))
import Sqel.Migration.Ddl (ToDdl)
import Sqel.Migration.Statement (migrationStatementSql, migrationsStatements, migrationsTableStatements)

data Thing =
  Thing1 { x :: Int, y :: Int }
  |
  Thing2 { z :: Int, a :: Int }
  deriving stock (Eq, Show, Generic)

type Dd_Thing_Old =
  IndexPrefix "old_sum_index__" (
    Sum [
      Con [Prim, Prim],
      Con [Prim, Prim]
    ]
  )

data Dat0 =
  Dat0 {
    name :: Text
  }
  deriving stock (Eq, Show, Generic)

type Table_Dat0 = ToDdl (Table "dat" Dat0 Gen)

table_Dat0 :: SqelFor Def Table_Dat0
table_Dat0 = sqel

data Dat1 =
  Dat1 {
    num :: Maybe Int,
    name :: Text,
    thing :: Thing
  }
  deriving stock (Eq, Show, Generic)

type Table_Dat1 =
  MigrationTable "dat" Dat1 (Prod [
    Prim,
    Prim,
    Merge (Name "thing" Dd_Thing_Old)
 ])

table_Dat1 :: SqelFor Def Table_Dat1
table_Dat1 = sqel

data Dat =
  Dat {
    num :: Maybe Int,
    name :: Text,
    thing :: Thing
  }
  deriving stock (Eq, Show, Generic)

type Dd_Thing =
  RenameIndex "old_sum_index__Thing" Gen

type Table_Dat =
  MigrationTable "dat" Dat (Prod [
    Prim,
    Prim,
    Merge (Name "thing" Dd_Thing)
  ])

table_Dat :: SqelFor Def Table_Dat
table_Dat = sqel

migrations :: Migrate Def Identity [Table_Dat, Table_Dat1, Table_Dat0]
migrations = table_Dat0 --> table_Dat1 --> table_Dat

stmtsTarget :: [[Sql]]
stmtsTarget =
  [
    [
      [exon|alter table "dat" rename column "old_sum_index__thing" to "sqel_sum_index__thing"|]
    ],
    [
      [exon|alter table "dat" add column "old_sum_index__thing" bigint|],
      [exon|alter table "dat" alter column "old_sum_index__thing" set not null|],
      [exon|alter table "dat" add column "thing1" "sqel_type__thing1"|],
      [exon|alter table "dat" alter column "thing1" set not null|],
      [exon|alter table "dat" add column "thing2" "sqel_type__thing2"|],
      [exon|alter table "dat" alter column "thing2" set not null|],
      [exon|alter table "dat" add column "num" bigint|],
      [exon|create type "sqel_type__thing1" as ("x" bigint, "y" bigint)|],
      [exon|create type "sqel_type__thing2" as ("z" bigint, "a" bigint)|]
    ]
  ]

tableStmtsTarget :: [[Sql]]
tableStmtsTarget =
  [
    [
      [exon|create table "dat" ("num" bigint, "name" text not null, "sqel_sum_index__thing" bigint not null, "thing1" "sqel_type__thing1" not null, "thing2" "sqel_type__thing2" not null)|],
      [exon|create type "sqel_type__thing1" as ("x" bigint, "y" bigint)|],
      [exon|create type "sqel_type__thing2" as ("z" bigint, "a" bigint)|]
    ],
    [
      [exon|create table "dat" ("num" bigint, "name" text not null, "old_sum_index__thing" bigint not null, "thing1" "sqel_type__thing1" not null, "thing2" "sqel_type__thing2" not null)|],
      [exon|create type "sqel_type__thing1" as ("x" bigint, "y" bigint)|],
      [exon|create type "sqel_type__thing2" as ("z" bigint, "a" bigint)|]
    ],
    [
      [exon|create table "dat" ("name" text not null)|]
    ]
  ]

test_migrationSum :: TestT IO ()
test_migrationSum = do
  stmtsTarget === (fmap migrationStatementSql <$> migrationsStatements migrations)
  tableStmtsTarget === migrationsTableStatements migrations
