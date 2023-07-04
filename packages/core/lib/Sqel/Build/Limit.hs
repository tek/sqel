module Sqel.Build.Limit where

import Sqel.Data.Field (PrimField (PrimField))
import qualified Sqel.Data.QueryMeta
import Sqel.Data.QueryMeta (QueryMeta (QueryMeta))
import Sqel.Data.Sql (Sql)
import qualified Sqel.Default
import Sqel.Default (Def, PrimMeta (PrimMeta))
import Sqel.Sql.Prepared (dollar)

limitClause :: Bool -> PrimField Def -> Sql
limitClause _ (PrimField PrimMeta {query = QueryMeta {index}}) =
  dollar index
limitClause _ _ =
  error "not implemented"
