module Sqel.Test.Statement.NullableQueryTest where

import Hedgehog (TestT, (===))

import Sqel.Column (nullable, orNull)
import Sqel.Data.Dd (Sqel, type (:>) ((:>)))
import Sqel.Data.QuerySchema (QuerySchema)
import Sqel.Data.Sql (Sql, sql)
import Sqel.Data.TableSchema (TableSchema)
import Sqel.PgType (tableSchema)
import Sqel.Prim (prim, primNewtype, prims)
import Sqel.Product (prod)
import Sqel.Query (checkQuery)
import Sqel.Query.Combinators (greaterEq)
import qualified Sqel.Sql.Select as Sql

data Dat =
  Dat {
    num :: Int,
    name :: Text
  }
  deriving stock (Eq, Show, Generic)

dd_Dat :: Sqel Dat _
dd_Dat =
  prod prims

newtype Numb =
  Numb { unNumb :: Int }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord)

data Q =
  Q {
    num :: Maybe Numb,
    name :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

target_nullable :: Sql
target_nullable =
  [sql|select "num", "name" from "dat" where (($1 is null or "num" >= $1) and ($2 is null or "name" = $2))|]

test_statement_nullableQuery :: TestT IO ()
test_statement_nullableQuery =
  target_nullable === Sql.selectWhere qs ts
  where
    ts :: TableSchema Dat
    ts = tableSchema dd_Dat
    qs :: QuerySchema Q Dat
    qs = checkQuery (prod (nullable (greaterEq primNewtype) :> nullable prim)) dd_Dat

target_orNull :: Sql
target_orNull =
  [sql|select "num", "name" from "dat" where ("num" >= $1 and "name" = $2)|]

test_statement_orNullQuery :: TestT IO ()
test_statement_orNullQuery =
  target_orNull === Sql.selectWhere qs ts
  where
    ts :: TableSchema Dat
    ts = tableSchema dd_Dat
    qs :: QuerySchema Q Dat
    qs = checkQuery (prod (orNull (greaterEq primNewtype) :> orNull prim)) dd_Dat
