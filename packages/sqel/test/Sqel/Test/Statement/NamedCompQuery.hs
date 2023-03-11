module Sqel.Test.Statement.NamedCompQuery where

import Hedgehog (TestT, (===))

import Sqel.Data.Dd (Sqel, type (:>) ((:>)))
import Sqel.Data.QuerySchema (QuerySchema)
import Sqel.Data.Sql (Sql, sql)
import Sqel.Data.TableSchema (TableSchema)
import Sqel.Names (named)
import Sqel.PgType (tableSchema)
import Sqel.Prim (prim)
import Sqel.Product (prod)
import Sqel.Query (checkQuery)
import qualified Sqel.Sql.Select as Sql

data Nc1 =
  Nc1 {
    name :: Text
  }
  deriving stock (Eq, Show, Generic)

data Nc =
  Nc {
    nc1 :: Nc1,
    cat :: Text
  }
  deriving stock (Eq, Show, Generic)

data Qn =
  Qn {
    name :: Text
  }
  deriving stock (Eq, Show, Generic)

dd_Nc :: Sqel Nc _
dd_Nc =
  prod (prod prim :> prim)

dd_Qn :: Sqel Qn _
dd_Qn =
  named @"nc1" (prod prim)

query_namedComp :: QuerySchema Qn Nc
query_namedComp =
  checkQuery dd_Qn dd_Nc

schema_namedComp :: TableSchema Nc
schema_namedComp =
  tableSchema dd_Nc

stmt_namedCompQuery :: Sql
stmt_namedCompQuery =
  Sql.selectWhere query_namedComp schema_namedComp

test_namedCompQuery :: TestT IO ()
test_namedCompQuery =
  [sql|select ("nc1").name, "cat" from "nc" where (("nc1")."name" = $1)|] === stmt_namedCompQuery
