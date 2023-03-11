module Sqel.Test.Statement.NewtypeArrayTest where

import Hedgehog (TestT, (===))

import Sqel.Column (nullable)
import Sqel.Data.Dd (Sqel)
import Sqel.Data.Sql (Sql, sql, toSql)
import Sqel.Data.SqlFragment (Select (Select))
import Sqel.PgType (tableSchema)
import Sqel.Prim (array, newtypeWrap, prim)
import Sqel.Product (prod)

newtype Nums =
  Nums { unNums :: Maybe [Int64] }
  deriving stock (Eq, Show, Generic)

data NtArray =
  NtArray {
    nums :: Nums
  }
  deriving stock (Eq, Show, Generic)

dd_newtypeArray :: Sqel NtArray _
dd_newtypeArray =
  prod (newtypeWrap (nullable (array prim)))

statement_newtypeArray :: Sql
statement_newtypeArray =
  toSql (Select (tableSchema dd_newtypeArray))

test_statement_newtypeArray :: TestT IO ()
test_statement_newtypeArray =
  [sql|select "nums" from "nt_array"|] === statement_newtypeArray
