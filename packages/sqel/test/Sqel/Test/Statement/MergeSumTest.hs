module Sqel.Test.Statement.MergeSumTest where

import Hedgehog (TestT, (===))
import Prelude hiding (sum)

import Sqel.Data.Dd (Dd, DdK (DdK), type (:>) ((:>)))
import Sqel.Data.Sql (Sql, sql, toSql)
import Sqel.Data.SqlFragment (Create (Create), Select (Select))
import Sqel.Data.Uid (Uid)
import Sqel.Merge (merge)
import Sqel.PgType (tableSchema)
import Sqel.Prim (prim, prims)
import Sqel.Product (prod)
import Sqel.Sum (ConColumn (con), sum)
import Sqel.Uid (uid)

target_mergeSum :: Sql
target_mergeSum =
  [sql|select "id", "sqel_sum_index__merge_sum", ("merge_sum1").num1, ("merge_sum1").name1, ("merge_sum2").num2,
       ("merge_sum2").name2 from "merge_sum"|]

target_create_mergeSum :: Sql
target_create_mergeSum =
  [sql|create table "merge_sum"
  ("id" bigint not null,
    "sqel_sum_index__merge_sum" bigint not null,
    "merge_sum1" sqel_type__merge_sum1 not null,
    "merge_sum2" sqel_type__merge_sum2 not null)
  |]

data MergeSum =
  MergeSum1 { num1 :: Int, name1 :: Text }
  |
  MergeSum2 { num2 :: Int, name2 :: Text }
  deriving stock (Eq, Show, Generic)

dd_uid_merge_sum_manual :: Dd ('DdK _ _ (Uid Int64 MergeSum) _)
dd_uid_merge_sum_manual =
  prod (prim :> merge (sum (con prims :> con prims)))

dd_uid_merge_sum :: Dd ('DdK _ _ (Uid Int64 MergeSum) _)
dd_uid_merge_sum =
  uid prim (sum (con prims :> con prims))

test_statement_merge_sum :: TestT IO ()
test_statement_merge_sum = do
  target_mergeSum === toSql (Select (tableSchema dd_uid_merge_sum))
  target_mergeSum === toSql (Select (tableSchema dd_uid_merge_sum_manual))
  target_create_mergeSum === toSql (Create (tableSchema dd_uid_merge_sum_manual))
