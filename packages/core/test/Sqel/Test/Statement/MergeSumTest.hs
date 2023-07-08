module Sqel.Test.Statement.MergeSumTest where

import Hedgehog (TestT, (===))

import Sqel.Class.ReifySqel (sqel)
import Sqel.Clauses (createTable, from, select)
import Sqel.Data.Sql (Sql, sql)
import Sqel.Data.Statement (statementSql)
import Sqel.Data.Uid (Uid)
import Sqel.Default (Sqel)
import Sqel.Dsl (Gen, Merge, Pk, Prim, Sum, Table, UidProd)
import Sqel.Syntax.Fragments (table)
import qualified Sqel.Syntax.Monad as S

target_mergeSum :: Sql
target_mergeSum =
  [sql|
  select "id",
         "sqel_sum_index__merge_sum",
         ("merge_sum1")."num1",
         ("merge_sum1")."name1",
         ("merge_sum2")."num2",
         ("merge_sum2")."name2"
  from "merge_sum"
  |]

target_create_mergeSum :: Sql
target_create_mergeSum =
  [sql|create table "merge_sum"
  ("id" bigint primary key not null,
    "sqel_sum_index__merge_sum" bigint not null,
    "merge_sum1" "sqel_type__merge_sum1" not null,
    "merge_sum2" "sqel_type__merge_sum2" not null)
  |]

data MergeSum =
  MergeSum1 { num1 :: Int, name1 :: Text }
  |
  MergeSum2 { num2 :: Int, name2 :: Text }
  deriving stock (Eq, Show, Generic)

type Table_MergeSum = Table "merge_sum" (Uid Int64 MergeSum) (UidProd (Pk Prim) (Merge (Sum [Gen, Gen])))

mergeSum :: Sqel Table_MergeSum
mergeSum = sqel

statement1 :: Sql
statement1 = statementSql S.do
  t <- table mergeSum
  select t
  from t

statement2 :: Sql
statement2 = statementSql @_ @_ @() S.do
  t <- table mergeSum
  createTable t

test_statement_merge_sum :: TestT IO ()
test_statement_merge_sum = do
  target_mergeSum === statement1
  target_create_mergeSum === statement2
