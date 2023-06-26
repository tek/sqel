module Sqel.Build.Offset where

import Sqel.Data.Field (PrimField (PrimField))
import Sqel.Data.Sql (Sql)
import qualified Sqel.Default
import Sqel.Default (Def, PrimMeta (PrimMeta), QueryMeta (QueryMeta))
import Sqel.Sql.Prepared (dollar)

offsetClause :: Bool -> PrimField Def -> Sql
offsetClause _ (PrimField (PrimMeta {query = QueryMeta {index}})) =
  dollar index
offsetClause _ _ =
  error "not implemented"
