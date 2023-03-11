module Sqel.Test.Statement.MergeProdTest where

import Hedgehog (TestT, (===))
import Prelude hiding (sum)

import Sqel.Data.Dd (Dd, DdK (DdK), type (:>) ((:>)))
import Sqel.Data.Sql (Sql, sql, toSql)
import Sqel.Data.SqlFragment (Select (Select))
import Sqel.Merge (merge)
import Sqel.PgType (tableSchema)
import Sqel.Prim (prim, prims)
import Sqel.Product (prod)
import Sqel.Test.Statement.Common (Pro)

data MergeProd =
  MergeProd { count :: Int, b :: Pro }
  deriving stock (Eq, Show, Generic)

dd_merge_prod :: Dd ('DdK _ _ MergeProd _)
dd_merge_prod =
  prod (prim :> merge (prod prims))

target_merge_prod :: Sql
target_merge_prod =
  [sql|select "count", "num", "name" from "merge_prod"|]

test_statement_merge_prod :: TestT IO ()
test_statement_merge_prod =
  target_merge_prod === toSql (Select (tableSchema dd_merge_prod))
