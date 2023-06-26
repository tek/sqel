module Sqel.Test.Statement.IgnoreTest where

import Hedgehog (TestT, (===))

import Sqel.Clauses (from, select, where_)
import Sqel.Data.Sql (Sql, sql)
import Sqel.Default (Def)
import Sqel.Dsl (Ignore, Prim, Prod, Query)
import Sqel.Syntax.Fragments (query1K)
import qualified Sqel.Syntax.Monad as S
import Sqel.Test.Statement.Common (Table_Simp)

data Q =
  Q {
    name :: Text,
    invalid :: Int64
  }
  deriving stock (Eq, Show, Generic)

type Query_Q = Query Q (Prod [Prim, Ignore Prim])

target_params :: Sql
target_params =
  [sql|select "name", "number" from "simp" where "name" = $1|]

test_statement_ignore :: TestT IO ()
test_statement_ignore =
  target_params === S.do
    frags <- query1K @Query_Q @Table_Simp @Def
    select frags.table
    from frags.table
    where_ frags.query
