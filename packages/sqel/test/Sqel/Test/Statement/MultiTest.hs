module Sqel.Test.Statement.MultiTest where

import Hedgehog (TestT, (===))
import Prelude hiding (sum)

import Sqel.Column (pk, unique)
import Sqel.Data.Dd (Sqel, type (:>) ((:>)))
import qualified Sqel.Data.QuerySchema as QuerySchema
import Sqel.Data.QuerySchema (QuerySchema)
import Sqel.Data.Sql (Sql, sql)
import Sqel.Data.SqlFragment (Update (Update))
import Sqel.Data.TableSchema (TableSchema)
import Sqel.Data.Uid (Uid)
import Sqel.Merge (merge)
import Sqel.PgType (tableSchema)
import Sqel.Prim (prim)
import Sqel.Product (prod)
import Sqel.Query (checkQuery)
import Sqel.Query.Combinators (set)
import Sqel.Uid (uid)

data Jn1 =
  Jn1 {
    j1a :: Text,
    j1b :: Text
  }
  deriving stock (Eq, Show, Generic)

data Jn2 =
  Jn2 {
    j2a :: Text,
    j2b :: Text
  }
  deriving stock (Eq, Show, Generic)

data Jq1 =
  Jq1 {
    j1a :: Text
  }
  deriving stock (Eq, Show, Generic)

data Jq =
  Jq {
    jq1 :: Jq1
  }
  deriving stock (Eq, Show, Generic)

dd_Jn1 :: Sqel (Uid Int Jn1) _
dd_Jn1 =
  uid (pk prim) (prod (unique prim :> prim))

schema_Jn1 :: TableSchema (Uid Int Jn1)
schema_Jn1 =
  tableSchema dd_Jn1

schema_Jn2 :: TableSchema (Uid Int Jn2)
schema_Jn2 =
  tableSchema (uid (pk prim) (prod (unique prim :> prim)))

dd_Jq1 :: Sqel Jq1 _
dd_Jq1 =
  prod (set prim)

dd_Jq :: Sqel Jq _
dd_Jq =
  prod (merge dd_Jq1)

query_Jq :: QuerySchema Jq (Uid Int Jn1)
query_Jq =
  checkQuery dd_Jq dd_Jn1

stmt_updateFrom :: Sql
stmt_updateFrom =
  [sql|##{Update schema_Jn1} ##{query_Jq.frags}|]

test_statement_updateFrom :: TestT IO ()
test_statement_updateFrom =
  [sql|
  update "jn1" j1
  set "j1a" = $1
  from "jn2" j2
  where "j2a" = $2 and j1.j1b = j2.j2b
  |] === stmt_updateFrom
