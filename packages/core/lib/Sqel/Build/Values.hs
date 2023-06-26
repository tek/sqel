module Sqel.Build.Values where

import Exon (exon)

import Sqel.Build.Index (prependIndex)
import Sqel.Data.Field (Field (Field))
import Sqel.Data.Spine (Spine (SpineMerge, SpineNest, SpinePrim), pattern SpineComp)
import Sqel.Data.Sql (Sql)
import qualified Sqel.Default
import Sqel.Default (Def, PrimMeta (PrimMeta), QueryMeta (QueryMeta), SpineDef)
import Sqel.Sql (joinComma)
import Sqel.Sql.Prepared (dollar)

valuePrim :: PrimMeta -> Maybe Sql
valuePrim = \case
  PrimMeta {query = QueryMeta {index}} -> Just (dollar index)
  _ -> Nothing

valuesComp :: [SpineDef] -> [Sql]
valuesComp cols =
  concatMap valuesSub cols

-- TODO use patsyn to extract cols with index
valuesSub :: Spine Def -> [Sql]
valuesSub = \case
  SpineNest _ compSort cols -> [[exon|row(#{joinComma (valuesComp (prependIndex compSort cols))})|]]
  SpineMerge _ compSort cols -> valuesComp (prependIndex compSort cols)
  SpinePrim meta -> maybeToList (valuePrim meta)

-- TODO would be nice if this could be guaranteed nonempty
-- maybe Spine could have NonEmpty?
fieldValues :: Field Def -> Maybe (NonEmpty Sql)
fieldValues (Field s) = case s of
  SpineComp _ _ cols -> nonEmpty (valuesComp cols)
  SpinePrim meta -> pure <$> valuePrim meta

valuesClause :: [Field Def] -> Sql
valuesClause fields =
  [exon|(#{cols})|]
  where
    cols = foldMap joinComma (mconcat (fieldValues <$> fields))
