module Sqel.Test.JsonTest where

import Data.Aeson (FromJSON, ToJSON)
import Hedgehog (TestT, (===))

import Sqel.Clauses (from, select)
import Sqel.Data.Sql (Sql, sql)
import Sqel.Data.Statement (statementSql)
import Sqel.Default (Def)
import Sqel.Dsl (Json, Prim, Prod, Table)
import Sqel.Syntax.Fragments (tableK)
import qualified Sqel.Syntax.Monad as S

data J1 =
  J1 {
    f3 :: Int,
    f4 :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data J =
  J {
    f1 :: Int,
    f2 :: J1
  }
  deriving stock (Eq, Show, Generic)

type Table_J =
  Table "j" J (Prod [Prim, Json])

statement_j :: Sql
statement_j = statementSql S.do
  t <- tableK @Table_J @Def
  select t
  from t

test_json :: TestT IO ()
test_json =
  [sql|select "f1", "f2" from "j"|] === statement_j
