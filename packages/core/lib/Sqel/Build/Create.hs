module Sqel.Build.Create where

import Exon (exon)

import Sqel.Build.Index (prependIndex)
import Sqel.Build.Table (tableName, typeName)
import Sqel.Data.Field (RootField (RootField), TypeField (TypeField))
import Sqel.Data.Spine (Spine (SpineMerge, SpineNest, SpinePrim), pattern SpineComp)
import Sqel.Data.Sql (Sql, sql, toSql)
import qualified Sqel.Default
import Sqel.Default (CompMeta, Def, PrimMeta, SpineDef)
import Sqel.Sql (joinComma)

createPrim :: Bool -> PrimMeta -> Sql
createPrim isTable meta =
  [sql|##{meta.name} ##{meta.colType} #{if isTable then toSql meta.constr else ""}|]

createComp :: Bool -> CompMeta -> Sql
createComp isTable meta =
  [sql|##{meta.name} ##{meta.colType} #{if isTable then toSql meta.constr else ""}|]

createSub :: Bool -> SpineDef -> [Sql]
createSub table = \case
  SpinePrim meta -> [createPrim table meta]
  SpineNest meta _ _ -> [createComp table meta]
  SpineMerge _ compSort cols -> createSub table =<< prependIndex compSort cols

create :: Bool -> SpineDef -> [Sql]
create table = \case
  SpinePrim meta -> [createPrim table meta]
  SpineComp _ compSort cols -> createSub table =<< prependIndex compSort cols

createCols :: RootField Def -> [Sql]
createCols (RootField s) = create True s

createTypeCols :: TypeField Def -> [Sql]
createTypeCols (TypeField s) = create False s

createTypeClause :: TypeField Def -> Sql
createTypeClause field@(TypeField s) =
  [exon|#{typeName s} as (#{cols})|]
  where
    cols = joinComma (createTypeCols field)

createTableClause :: RootField Def -> Sql
createTableClause field =
  [exon|#{tableName field} (#{cols})|]
  where
    cols = joinComma (createCols field)
