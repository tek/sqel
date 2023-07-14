module Sqel.Build.Table where

import Data.Some (Some (Some))
import Exon (exon)

import Sqel.Data.Field (Field (Field), RootField (RootField))
import Sqel.Data.PgType (PgColumnName (PgColumnName))
import Sqel.Data.PgTypeName (pattern PgTableName, pattern PgTypeName)
import Sqel.Data.Spine (Spine (SpinePrim), pattern SpineComp)
import Sqel.Data.Sql (Sql, sqlQuote, toSql)
import qualified Sqel.Default
import Sqel.Default (CompMeta (CompMeta), Def, PrimMeta (PrimMeta))

-- TODO type directed quoting
typeNameText :: Spine Def -> Text
typeNameText = \case
  SpineComp CompMeta {typeName = Some (PgTypeName name)} _ _ -> name
  SpinePrim _ PrimMeta {name = PgColumnName name} -> name

tableNameText :: RootField Def -> Text
tableNameText (RootField (Field _ s)) = case s of
  SpineComp CompMeta {typeName = Some (PgTypeName name)} _ _ -> name
  SpinePrim (PgTableName name) _ -> name

typeName :: Spine Def -> Sql
typeName = \case
  SpineComp CompMeta {typeName = Some name} _ _ -> toSql name
  SpinePrim _ PrimMeta {name} -> toSql name

tableName :: RootField Def -> Sql
tableName (RootField (Field _ s)) = case s of
  SpineComp CompMeta {typeName = Some name} _ _ -> toSql name
  SpinePrim table _ -> toSql table

bindClause :: Bool -> RootField Def -> Sql
bindClause multi (tableNameText -> name) =
  [exon|#{sqlQuote name}##{alias}|]
  where
    alias | multi = [exon| #{name}|]
          | otherwise = ""
