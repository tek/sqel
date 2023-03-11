module Sqel.Test.Statement.OrderTest where

import Hedgehog (TestT, (===))

import Sqel.Data.Order (Order (Desc))
import Sqel.Data.QuerySchema (QuerySchema)
import Sqel.Data.Sql (Sql, sql)
import Sqel.Data.TableSchema (TableSchema)
import Sqel.PgType (tableSchema)
import Sqel.Prim (prim)
import Sqel.Product (prod)
import Sqel.Query (checkQuery)
import Sqel.Query.Combinators (order)
import qualified Sqel.Sql.Select as Sql

data Dat =
  Dat {
    num :: Int
  }
  deriving stock (Eq, Show, Generic)

data Q =
  Q {
    num :: ()
  }
  deriving stock (Eq, Show, Generic)

target_order :: Sql
target_order =
  [sql|select "num" from "dat" order by "num" desc|]

test_statement_order :: TestT IO ()
test_statement_order =
  target_order === Sql.selectWhere qs ts
  where
    ts :: TableSchema Dat
    ts = tableSchema (prod prim)
    qs :: QuerySchema Q Dat
    qs = checkQuery (prod (order Desc)) (prod prim)
