module Sqel.Test.Statement.CondOpTest where

import Exon (exon)
import Hedgehog (TestT, (===))

import Sqel.Class.ReifySqel (sqel)
import Sqel.Data.Sql (Sql)
import qualified Sqel.Data.Statement as Statement
import Sqel.Data.Statement (Statement)
import Sqel.Default (Sqel)
import Sqel.Dsl (GEq, Lt, Prim, Prod, Query)
import qualified Sqel.Statement.Common as Statement
import Sqel.Test.Statement.Common (Simp, table_Simp)

data Q =
  Q {
    name :: Text,
    number :: Int64
  }
  deriving stock (Eq, Show, Generic)

type Query_Q = Query Q (Prod [Lt Prim, GEq Prim])

query_Q :: Sqel Query_Q
query_Q = sqel

statement :: Statement Q Simp
statement = Statement.selectWhere query_Q table_Simp

target :: Sql
target = [exon|select "name", "number" from "simp" where "name" < $1 and "number" >= $2|]

test_condOp :: TestT IO ()
test_condOp =
  target === statement.sql
