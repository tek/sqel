module Sqel.Test.Statement.ParamsTest where

import Hedgehog (TestT, (===))

import Sqel.Clauses (from, limit, offset, select, where_)
import Sqel.Data.Sql (Sql, sql)
import Sqel.Default (Def)
import Sqel.Dsl (OrNull, Param, Prim, Prod, Query)
import Sqel.Syntax.Fragments (query1K)
import qualified Sqel.Syntax.Monad as S
import Sqel.Test.Statement.Common (Table_Simp)

data Q =
  Q {
    name :: Text,
    limit :: Int64,
    offset :: Maybe Int64
  }
  deriving stock (Eq, Show, Generic)

type Query_Q = Query Q (Prod [Prim, Param Prim, Param (OrNull Prim)])

target_params :: Sql
target_params =
  [sql|select "name", "number" from "simp" where "name" = $1 limit $2 offset $3|]

test_statement_params :: TestT IO ()
test_statement_params =
  target_params === S.do
    frags <- query1K @Query_Q @Table_Simp @Def
    select frags.table
    from frags.table
    where_ frags.query
    limit frags.query.limit
    offset frags.query.offset
