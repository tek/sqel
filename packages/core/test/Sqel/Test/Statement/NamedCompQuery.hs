module Sqel.Test.Statement.NamedCompQuery where

import Generics.SOP (NP (Nil, (:*)))
import Hedgehog (TestT, (===))

import Sqel.Class.ReifySqel (sqel)
import Sqel.Clauses (from, select, where_)
import Sqel.Data.Sql (Sql, sql)
import Sqel.Default (Sqel)
import Sqel.Dsl
import Sqel.Syntax.Fragments (query)
import qualified Sqel.Syntax.Monad as S

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

type Table_Nc = Table "nc" Nc (Prod [Gen, Prim])

type Query_Qn = Query Qn (Name "nc1" Gen)

sqel_Nc :: Sqel Table_Nc
sqel_Nc = sqel

sqel_Qn :: Sqel Query_Qn
sqel_Qn = sqel

stmt_namedCompQuery :: Sql
stmt_namedCompQuery = S.do
  fs <- query sqel_Qn (sqel_Nc :* Nil)
  select fs.table
  from fs.table
  where_ fs.query

test_namedCompQuery :: TestT IO ()
test_namedCompQuery =
  [sql|select ("nc1")."name", "cat" from "nc" where ("nc1")."name" = $1|] === stmt_namedCompQuery
