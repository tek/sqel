module Sqel.Build.Order where

import Exon (exon)

import Sqel.Data.Clause (ClauseParam (ClauseParam))
import Sqel.Data.Def (Def)
import Sqel.Data.Field (PrimField (PrimField))
import Sqel.Data.Order (Order)
import Sqel.Data.Sql (Sql)
import Sqel.Path (primMetaPath, primSelector)

-- TODO order by comp?
orderByClause :: Bool -> (ClauseParam (PrimField Def) Order) -> Sql
orderByClause multi (ClauseParam (PrimField _ table meta) order) =
  [exon|##{sel} ##{order}|]
  where
    sel = primSelector (primMetaPath multi table meta)
