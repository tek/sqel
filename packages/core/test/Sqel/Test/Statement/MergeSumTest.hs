module Sqel.Test.Statement.MergeSumTest where

import Hedgehog (TestT, (===))

import Sqel.Class.ReifySqel (sqel)
import Sqel.Data.Sql (Sql, sql)
import Sqel.Data.Statement (statementSql)
import Sqel.Data.TestTables (SI, Type_SI)
import Sqel.Data.Uid (Uid)
import Sqel.Default (Sqel)
import Sqel.Dsl (Gen, Merge, Pk, Prim, Prod, Sum, Table, UidProd)
import qualified Sqel.Statement.Common as Statement

data MergeSum =
  MergeSum1 { num1 :: Int64, name1 :: Text }
  |
  MergeSum2 { num2 :: Int64, name2 :: Text }
  deriving stock (Eq, Show, Generic)

type Table_MergeSum = Table "merge_sum" (Uid Int64 MergeSum) (UidProd (Pk Prim) (Merge (Sum [Gen, Gen])))

table_MergeSum :: Sqel Table_MergeSum
table_MergeSum = sqel

data MergeCon1 =
  MergeCon1 { si :: SI, num :: Int64 }
  deriving stock (Eq, Show, Generic)

type Table_MergeCon1 = Table "merge_con1" MergeCon1 (Prod [Merge Type_SI, Prim])

table_MergeCon1 :: Sqel Table_MergeCon1
table_MergeCon1 = sqel

target_MergeSum :: Sql
target_MergeSum =
  [sql|
  select "id",
         "sqel_sum_index__merge_sum",
         ("merge_sum1")."num1",
         ("merge_sum1")."name1",
         ("merge_sum2")."num2",
         ("merge_sum2")."name2"
  from "merge_sum"
  |]

target_create_MergeSum :: Sql
target_create_MergeSum =
  [sql|create table "merge_sum"
  ("id" bigint primary key not null,
    "sqel_sum_index__merge_sum" bigint not null,
    "merge_sum1" "sqel_type__merge_sum1" not null,
    "merge_sum2" "sqel_type__merge_sum2" not null)
  |]

target_insert_MergeSum :: Sql
target_insert_MergeSum =
  [sql|
  insert into "merge_sum" ("id", "sqel_sum_index__merge_sum", "merge_sum1", "merge_sum2")
  values ($1, $2, row($3, $4), row($5, $6))
  |]

target_create_MergeCon1 :: Sql
target_create_MergeCon1 =
  [sql|
  create table "merge_con1"
  ("sqel_sum_index__si" bigint not null,
  "si1" bigint,
  "si2" bigint,
  "num" bigint not null)
  |]

test_statement_merge_sum :: TestT IO ()
test_statement_merge_sum = do
  -- target_MergeSum === statementSql (Statement.selectAll table_MergeSum)
  -- target_create_MergeSum === statementSql (Statement.createTable table_MergeSum)
  -- target_insert_MergeSum === statementSql (Statement.insert table_MergeSum)
  target_create_MergeCon1 === statementSql (Statement.createTable table_MergeCon1)
