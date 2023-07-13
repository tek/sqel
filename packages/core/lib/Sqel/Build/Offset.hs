module Sqel.Build.Offset where

import Sqel.Data.Field (PrimField (PrimField))
import qualified Sqel.Data.QueryMeta
import Sqel.Data.QueryMeta (QueryMeta (QueryMeta))
import Sqel.Data.Sql (Sql)
import qualified Sqel.Default
import Sqel.Default (Def, PrimMeta (PrimMeta))
import Sqel.Sql.Prepared (dollar)

-- TODO what do
offsetClause :: Bool -> PrimField Def -> Sql
offsetClause _ (PrimField True (PrimMeta {query = QueryMeta {index}})) =
  dollar index
offsetClause _ _ =
  error "not implemented"
