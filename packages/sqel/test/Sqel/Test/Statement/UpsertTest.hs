module Sqel.Test.Statement.UpsertTest where

import Hedgehog (TestT, (===))
import Prelude hiding (sum)

import Sqel.Column (pk, unique)
import Sqel.Data.Dd (Sqel, type (:>) ((:>)))
import Sqel.Data.Sql (sql)
import qualified Sqel.Data.TableSchema as TableSchema
import Sqel.Data.TableSchema (TableSchema)
import Sqel.Data.Uid (Uid)
import Sqel.PgType (tableSchema)
import Sqel.Prim (prim)
import Sqel.Product (prod)
import Sqel.Statement (upsertSql)
import Sqel.Sum (con1, sum)
import Sqel.Uid (uid)

data Ups =
  Ups {
    uni :: Text,
    equi :: Text
  }
  deriving stock (Eq, Show, Generic)

schema_Ups :: TableSchema (Uid Int Ups)
schema_Ups =
  tableSchema (uid (pk prim) (prod (unique prim :> prim)))

test_statement_upsert :: TestT IO ()
test_statement_upsert =
  [sql|
  insert into "ups" ("id", "uni", "equi")
  values ($1, $2, $3)
  on conflict ("id", "uni")
  do update set "id" = $1, "uni" = $2, "equi" = $3|] === upsertSql schema_Ups.pg

data SumKey =
  SK1 { sk1 :: Int }
  |
  SK2 { sk2 :: Int }
  deriving stock (Eq, Show, Generic)

dd_SumKey :: Sqel SumKey _
dd_SumKey =
  sum (con1 prim :> con1 prim)

schema_Ups_sumKey :: TableSchema (Uid SumKey Ups)
schema_Ups_sumKey =
  tableSchema (uid (pk dd_SumKey) (prod (prim :> prim)))

test_statement_upsert_sumKey :: TestT IO ()
test_statement_upsert_sumKey =
  [sql|
  insert into "ups" ("id", "uni", "equi")
  values (row($1, $2, $3), $4, $5)
  on conflict ("id")
  do update set "id" = row($1, $2, $3), "uni" = $4, "equi" = $5|] === upsertSql schema_Ups_sumKey.pg
