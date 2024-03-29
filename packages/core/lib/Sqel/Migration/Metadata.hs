module Sqel.Migration.Metadata where

import qualified Data.Map.Merge.Strict as Map
import Data.Map.Merge.Strict (mapMaybeMissing, mapMissing, zipWithMaybeMatched)
import qualified Data.Map.Strict as Map
import Exon (exon)
import Prettyprinter (Pretty, pretty, vsep, (<+>))

import Sqel.Build.Index (prependIndexWith)
import Sqel.Class.DefaultFields (DefaultMeta (defaultCompMeta, defaultPrimMeta), defaultSort)
import qualified Sqel.Class.MigrationEffect as MigrationEffect
import Sqel.Class.MigrationEffect (MigrationEffect)
import qualified Sqel.Data.Constraints
import qualified Sqel.Data.ExistingColumn as ExistingColumn
import Sqel.Data.ExistingColumn (ExistingColumn (ExistingColumn))
import Sqel.Data.PgType (PgColumnName (PgColumnName), PgPrimName (PgPrimName), PgTypeRef (PgTypeRef))
import Sqel.Data.PgTypeName (pattern PgCompName, PgTableName, pattern PgTableName, pattern PgTypeName, PgTypeName)
import Sqel.Data.Spine (Spine (SpineMerge, SpineNest, SpinePrim))
import Sqel.Data.Sql (Sql)
import qualified Sqel.Default
import Sqel.Migration.Data.TypeStatus (
  ColumnMismatch (ColumnMismatch),
  MismatchReason (ExtraneousColumn, MissingColumn, TypeMismatch),
  TypeStatus (Absent, Match, Mismatch),
  )
import qualified Sqel.Statement.PgSchema as Statement
import Sqel.Statement.PgSchema (tableColumnsSql)

newtype DbCols =
  DbCols { unDbCols :: Map PgColumnName (Either PgTypeRef PgPrimName, Bool) }
  deriving stock (Eq, Show, Generic)

newtype PrettyColMap =
  PrettyColMap { unPrettyColMap :: DbCols }
  deriving stock (Eq, Show, Generic)

instance Pretty PrettyColMap where
  pretty (PrettyColMap (DbCols cols)) =
    vsep (uncurry col <$> Map.toList cols)
    where
      col name (tpe, nl) = pre name tpe <+> (if nl then "nullable" else "not nullable")
      pre name = \case
        Right tpe -> "*" <+> pretty name <+> pretty tpe
        Left ref -> "+" <+> pretty name <+> pretty ref

typeColumns ::
  Monad m =>
  MigrationEffect m =>
  Sql ->
  PgTypeName table ->
  m DbCols
typeColumns code (PgTypeName targetName) = do
  cols <- traverse mktype =<< MigrationEffect.dbCols targetName (Statement.dbColumns code)
  pure (DbCols (Map.fromList cols))
  where
    mktype = \case
      ExistingColumn {dataType = "USER-DEFINED", ..} ->
        pure (PgColumnName name, (Left (PgTypeRef udtName), isNullable))
      ExistingColumn {dataType = "ARRAY", elementDataType = Just el, ..} ->
        pure (PgColumnName name, (Right (PgPrimName [exon|#{el}[]|]), isNullable))
      ExistingColumn {elementDataType = Nothing, ..} ->
        pure (PgColumnName name, (Right (PgPrimName dataType), isNullable))
      ExistingColumn {elementDataType = Just el, ..} -> do
        MigrationEffect.error [exon|Error: non-array column with element type: #{name}/##{dataType} | ##{el}|]
        pure (PgColumnName name, (Right (PgPrimName dataType), isNullable))

tableColumns ::
  Monad m =>
  MigrationEffect m =>
  PgTableName ->
  m DbCols
tableColumns =
  typeColumns tableColumnsSql

columnMap ::
  ∀ tag .
  DefaultMeta tag =>
  [Spine tag] ->
  DbCols
columnMap =
  DbCols . Map.fromList . spin
  where
    spin = concatMap \case
      SpinePrim table (defaultPrimMeta @tag -> meta) -> [prim table meta]
      SpineNest (defaultCompMeta @tag -> meta) _ _ -> [(meta.name, (Left meta.colType, meta.constr.nullable))]
      SpineMerge (defaultCompMeta @tag -> _) (defaultSort -> compSort) cols -> withIndex compSort (spin cols)
    withIndex =
      prependIndexWith prim
    prim _ meta =
      (meta.name, (Right meta.colType, meta.constr.nullable))

pgKind :: PgTypeName table -> Text
pgKind = \case
  PgTableName _ -> "table"
  PgCompName _ -> "type"

onlyExtraneousNullable ::
  DbCols ->
  DbCols ->
  Bool
onlyExtraneousNullable (DbCols dbCols) (DbCols targetCols) =
  all snd (Map.elems rest)
  where
    rest = Map.differenceWith ignoreNullable dbCols targetCols
    ignoreNullable (l, n) (r, _) | l == r = Nothing
                                 | otherwise = Just (l, n)

checkMismatch ::
  DbCols ->
  DbCols ->
  Maybe (NonEmpty ColumnMismatch)
checkMismatch (DbCols dbCols) (DbCols targetCols) =
  nonEmpty $
  Map.elems $
  Map.merge (mapMaybeMissing extraneous) (mapMissing missing) (zipWithMaybeMatched match) dbCols targetCols
  where
    extraneous name = \case
      (_, True) -> Nothing
      (typeDb, False) -> Just (ColumnMismatch name typeDb ExtraneousColumn)
    missing name (typeTarget, _) = ColumnMismatch name typeTarget MissingColumn
    match name (typeDb, _) (typeTarget, _)
      | typeDb == typeTarget = Nothing
      | otherwise = Just (ColumnMismatch name typeDb TypeMismatch)

logType ::
  MigrationEffect m =>
  PgTypeName table ->
  TypeStatus ->
  DbCols ->
  DbCols ->
  m ()
logType name status dbCols targetCols =
  MigrationEffect.log (message status)
  where
    message = \case
      Absent ->
        [exon|Skipping column analysis for nonexistent #{k} '#{n}'|]
      Match ->
        [exon|DB #{k} '#{n}' matches|]
      Mismatch _ ->
        [exon|Trying migration for declared #{k} '#{n}' with:
#{show (pretty (PrettyColMap targetCols))}
for database #{k} with
#{show (pretty (PrettyColMap dbCols))}|]
    PgTypeName n = name
    k = pgKind name

typeStatus ::
  DbCols ->
  DbCols ->
  TypeStatus
typeStatus dbCols targetCols
  | Map.null ((.unDbCols) dbCols) = Absent
  | Just mismatches <- checkMismatch dbCols targetCols = Mismatch (toList mismatches)
  | otherwise = Match
