module Sqel.Build.Order where

import Exon (exon)

import Sqel.Data.Clause (ClauseParam (ClauseParam))
import Sqel.Data.Field (PrimField (PrimField))
import Sqel.Data.Order (Order)
import Sqel.Data.Sql (Sql)
import Sqel.Default (Def)
import Sqel.Path (primMetaPath, primSelector)

-- TODO order by comp?
orderByClause :: Bool -> (ClauseParam (PrimField Def) Order) -> Sql
orderByClause multi (ClauseParam (PrimField meta) order) =
  [exon|##{sel} ##{order}|]
  where
    sel = primSelector (primMetaPath multi meta)
