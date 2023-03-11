module Sqel.Test.Statement.Con1Test where

import Hedgehog (TestT, (===))
import Prelude hiding (sum)

import Sqel.Data.Dd (Dd, DdK (DdK), type (:>) ((:>)))
import Sqel.Data.Sql (Sql, sql, toSql)
import Sqel.Data.SqlFragment (Select (Select))
import Sqel.PgType (tableSchema)
import Sqel.Prim (prim)
import Sqel.Sum (con1, sum)

data NaNu =
  Na { name :: Text }
  |
  Nu Int64
  deriving stock (Eq, Show, Generic)

ddNaNu :: Dd ('DdK _ _ NaNu _)
ddNaNu =
  sum (con1 prim :> con1 prim)

statement_con1 :: Sql
statement_con1 =
  toSql (Select (tableSchema ddNaNu))

test_statement_con1 :: TestT IO ()
test_statement_con1 =
  [sql|select "sqel_sum_index__na_nu", "name", "nu" from "na_nu"|] === statement_con1
