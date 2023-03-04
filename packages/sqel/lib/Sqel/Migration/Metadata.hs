module Sqel.Migration.Metadata where

import qualified Data.Map.Strict as Map
import Exon (exon)
import Prettyprinter (Pretty, pretty, vsep, (<+>))

import qualified Sqel.Class.MigrationEffect as MigrationEffect
import Sqel.Class.MigrationEffect (MigrationEffect)
import qualified Sqel.Data.PgType as PgType
import Sqel.Data.PgType (
  ColumnType,
  PgColumn (PgColumn),
  PgColumnName (PgColumnName),
  PgPrimName (PgPrimName),
  PgTypeRef (PgTypeRef),
  )
import Sqel.Data.PgTypeName (
  pattern PgCompName,
  PgTableName,
  pattern PgTableName,
  pattern PgTypeName,
  PgTypeName,
  getPgTypeName,
  )
import Sqel.Data.Sql (Sql)
import Sqel.Migration.Data.TypeStatus (TypeStatus (Absent, Match, Mismatch))
import qualified Sqel.Statement as Statement
import Sqel.Statement (tableColumnsSql)

newtype DbCols =
  DbCols { unDbCols :: Map PgColumnName (Either PgTypeRef PgPrimName) }
  deriving stock (Eq, Show, Generic)

newtype PrettyColMap =
  PrettyColMap { unPrettyColMap :: DbCols }
  deriving stock (Eq, Show, Generic)

instance Pretty PrettyColMap where
  pretty (PrettyColMap (DbCols cols)) =
    vsep (uncurry col <$> Map.toList cols)
    where
      col name = \case
        Right tpe -> "*" <+> pretty name <+> pretty tpe
        Left ref -> "+" <+> pretty name <+> pretty ref

typeColumns ::
  Monad m =>
  MigrationEffect m =>
  Sql ->
  PgTypeName table ->
  m DbCols
typeColumns code (PgTypeName name) = do
  cols <- traverse mktype =<< MigrationEffect.dbCols name (Statement.dbColumns code)
  pure (DbCols (Map.fromList cols))
  where
    mktype = \case
      (col, "USER-DEFINED", n, _) ->
        pure (PgColumnName col, Left (PgTypeRef n))
      (col, "ARRAY", _, Just n) ->
        pure (PgColumnName col, Right (PgPrimName [exon|#{n}[]|]))
      (col, n, _, Nothing) ->
        pure (PgColumnName col, Right (PgPrimName n))
      (col, n, _, Just e) -> do
        MigrationEffect.error [exon|Error: non-array column with element type: ##{n} | ##{e}|]
        pure (PgColumnName col, Right (PgPrimName n))

tableColumns ::
  Monad m =>
  MigrationEffect m =>
  PgTableName ->
  m DbCols
tableColumns =
  typeColumns tableColumnsSql

columnMap :: [PgColumn] -> Map PgColumnName ColumnType
columnMap =
  Map.fromList . fmap \ PgColumn {name, pgType} -> (name, pgType)

pgKind :: PgTypeName table -> Text
pgKind = \case
  PgTableName _ -> "table"
  PgCompName _ -> "type"

logType ::
  MigrationEffect m =>
  PgTypeName table ->
  DbCols ->
  DbCols ->
  m ()
logType name dbCols colsByName
  | null (unDbCols dbCols) =
    MigrationEffect.log [exon|Skipping nonexistent #{k} '#{getPgTypeName name}'|]
  | otherwise =
    MigrationEffect.log [exon|Trying #{k} '#{getPgTypeName name}' with:
#{show (pretty (PrettyColMap colsByName))}
for existing #{k} with
#{show (pretty (PrettyColMap dbCols))}|]
  where
    k = pgKind name

typeStatus ::
  DbCols ->
  DbCols ->
  TypeStatus
typeStatus (DbCols dbCols) (DbCols colByName)
  | Map.null dbCols = Absent
  | dbCols == colByName = Match
  | otherwise = Mismatch
