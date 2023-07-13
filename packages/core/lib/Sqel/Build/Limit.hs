module Sqel.Build.Limit where

import Sqel.Data.Field (OrLiteral (LiteralField, NotLiteral), PrimField (PrimField))
import qualified Sqel.Data.QueryMeta
import Sqel.Data.QueryMeta (QueryMeta (QueryMeta))
import Sqel.Data.Sql (Sql (Sql))
import qualified Sqel.Default
import Sqel.Default (Def, PrimMeta (PrimMeta))
import Sqel.Sql.Prepared (dollar)

-- TODO :(
limitClause :: Bool -> OrLiteral Int64 (PrimField Def) -> Sql
limitClause _ (NotLiteral (PrimField True PrimMeta {query = QueryMeta {index}})) =
  dollar index
limitClause _ (LiteralField n) =
  Sql (show n)
limitClause _ _ =
  error "not implemented"
