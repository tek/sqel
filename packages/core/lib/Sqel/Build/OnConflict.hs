module Sqel.Build.OnConflict where

import Exon (exon)

import Sqel.Build.Columns (fieldColumnsWhere)
import Sqel.Constraints (constrUnique)
import Sqel.Data.Field (Field)
import Sqel.Data.Spine (Spine (SpinePrim), pattern SpineComp)
import Sqel.Data.Sql (Sql)
import qualified Sqel.Default as Default
import Sqel.Default (Def)
import Sqel.Sql (concatMapComma)

spineUnique :: Spine Def -> Bool
spineUnique = \case
  SpinePrim meta -> constrUnique meta.constr
  SpineComp meta _ _ -> constrUnique meta.constr

-- | This only uses base columns because composite type columns cannot have constraints like @unique@.
onConflictClause :: [Field Def] -> Sql
onConflictClause fields =
  [exon|(#{cols})|]
  where
    cols = concatMapComma (fieldColumnsWhere spineUnique) fields
