module Sqel.Test.Statement.QueryPathTest where

import Generics.SOP (NP (Nil, (:*)))
import Hedgehog (TestT, (===))

import Sqel.Class.ReifySqel (sqel)
import Sqel.Clauses (from, select, where_)
import Sqel.Data.Sql (Sql, sql)
import Sqel.Data.Statement (statementSql)
import Sqel.Default (Sqel)
import Sqel.Dsl (Gen, Path, Prim, Prod, Query, Table)
import Sqel.Syntax.Fragments (query)
import qualified Sqel.Syntax.Monad as S

data Dat2 =
  Dat2 {
    name :: Text
  }
  deriving stock (Eq, Show, Generic)

data Dat1 =
  Dat1 {
    dat2 :: Dat2
  }
  deriving stock (Eq, Show, Generic)

data Dat =
  Dat {
    dat1 :: Dat1,
    cat :: Text
  }
  deriving stock (Eq, Show, Generic)

data Q =
  Q {
    thing :: Text
  }
  deriving stock (Eq, Show, Generic)

type Table_Dat = Table "dat" Dat (Prod [Prod '[Gen], Prim])

type Query_Q = Query Q (Prod '[Path ["name", "dat2", "dat1"] Prim])

sqel_Dat :: Sqel Table_Dat
sqel_Dat = sqel

sqel_Q :: Sqel Query_Q
sqel_Q = sqel

stmt_queryPath :: Sql
stmt_queryPath = statementSql S.do
  fs <- query sqel_Q (sqel_Dat :* Nil)
  select fs.table
  from fs.table
  where_ fs.query

test_queryPath :: TestT IO ()
test_queryPath =
  [sql|select ("dat1")."dat2"."name", "cat" from "dat" where ("dat1")."dat2"."name" = $1|] === stmt_queryPath
