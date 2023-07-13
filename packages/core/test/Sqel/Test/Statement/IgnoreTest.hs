module Sqel.Test.Statement.IgnoreTest where

import Hedgehog (TestT, (===))

import Sqel.Clauses (from, select, where_)
import Sqel.Data.Def (Def)
import Sqel.Data.Sql (Sql, sql)
import Sqel.Data.Statement (statementSql)
import Sqel.Data.TestTables (Table_Simp)
import Sqel.Dsl (Ignore, Prim, Prod, Query)
import Sqel.Syntax.Fragments (query1K)
import qualified Sqel.Syntax.Monad as S

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
  target_params === statementSql S.do
    frags <- query1K @Query_Q @Table_Simp @Def
    select frags.table
    from frags.table
    where_ frags.query
