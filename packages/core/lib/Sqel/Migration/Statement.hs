module Sqel.Migration.Statement where

import Control.Monad.Trans.Writer.Strict (Writer, runWriterT, tell)
import qualified Data.Map.Strict as Map
import qualified Exon
import Generics.SOP (NP, SListI, htraverse_)
import Hasql.Encoders (Params)
import Hasql.Session (Session)
import qualified Text.Show as Show

import Sqel.Build.Sql (BuildClause)
import Sqel.Class.DefaultFields (DefaultMeta (defaultCompMeta, defaultPrimMeta), defaultSort)
import Sqel.Data.Constraints (renderConstraints)
import qualified Sqel.Data.Migration as Migration
import Sqel.Data.Migration (
  ColumnAction (AddColumn, RemoveColumn, RenameColumn, RenameColumnType),
  Mig (Mig),
  Migration (Migration),
  MigrationActions (AutoActions, CustomActions),
  TableDdl,
  TypeAction (AddAction, ModifyAction, RenameAction),
  tableDdlCurrent,
  )
import Sqel.Data.PgType (pattern PgColumnName)
import Sqel.Data.PgTypeName (
  pattern PgCompName,
  PgTableName,
  pattern PgTableName,
  pattern PgTypeName,
  PgTypeName,
  pgTableName,
  )
import Sqel.Data.Spine (CompSort (CompSum))
import Sqel.Data.Sqel (SqelFor (SqelPrim), pattern SqelMerge, pattern SqelNest)
import Sqel.Data.Sql (Sql, sql)
import Sqel.Data.Statement (Statement (Statement), statementSql)
import qualified Sqel.Default
import Sqel.Default (CompMeta, CreateTable, CreateType, Def, PrimMeta)
import Sqel.Migration.Fold (foldMigrations)
import Sqel.Sqel (foldTypes, sqelTableName)
import Sqel.Statement (runUnprepared)
import qualified Sqel.Statement.Common as Statement

data MigrationStatement where
  MigrationStatement :: p -> Params p -> Sql -> MigrationStatement

instance Show MigrationStatement where
  show (MigrationStatement _ _ s) = show s

migrationStatementSql :: MigrationStatement -> Sql
migrationStatementSql (MigrationStatement _ _ s) = s

alterStatement ::
  PgTypeName table ->
  (Sql -> Sql -> Sql) ->
  Writer [MigrationStatement] ()
alterStatement typeName f =
  tell [MigrationStatement () mempty (f [sql|alter #{entity} ##{pgTableName name}|] attr)]
  where
    (entity, attr, name) = case typeName of
      PgTableName n -> ("table", "column", n)
      PgCompName n -> ("type", "attribute", n)

addPrim ::
  ∀ table .
  PgTypeName table ->
  PrimMeta ->
  Maybe Text ->
  Writer [MigrationStatement] ()
addPrim typeName meta md = do
  alter_ \ alter attr -> [sql|#{alter} add #{attr} ##{colName} ##{colTypeName}|]
  case typeName of
    PgTableName _ -> do
      for_ md \ defVal -> do
        alterStatement typeName \ _ _ -> [sql|update ##{comp} set ##{colName} = ##{defVal}|]
      for_ (nonEmpty constr) \ opt ->
        alter_ \ alter attr ->
          [sql|#{alter} alter #{attr} ##{colName} set #{Exon.intercalate " " opt}|]
    PgCompName _ -> unit
  where
    colName = meta.name
    constr = renderConstraints meta.constr
    colTypeName = meta.colType
    alter_ = alterStatement typeName
    PgTypeName comp = typeName

addComp ::
  ∀ table .
  PgTypeName table ->
  CompMeta ->
  Maybe Text ->
  Writer [MigrationStatement] ()
addComp typeName meta md = do
  alter_ \ alter attr -> [sql|#{alter} add #{attr} ##{colName} ##{colTypeName}|]
  case typeName of
    PgTableName _ -> do
      for_ md \ defVal -> do
        alterStatement typeName \ _ _ -> [sql|update ##{comp} set ##{colName} = ##{defVal}|]
      for_ (nonEmpty constr) \ opt ->
        alter_ \ alter attr ->
          [sql|#{alter} alter #{attr} ##{colName} set #{Exon.intercalate " " opt}|]
    PgCompName _ -> unit
  where
    colName = meta.name
    constr = renderConstraints meta.constr
    colTypeName = meta.colType
    alter_ = alterStatement typeName
    PgTypeName comp = typeName

addCol ::
  ∀ tag table s .
  DefaultMeta tag =>
  PgTypeName table ->
  Maybe Text ->
  SqelFor tag s ->
  Writer [MigrationStatement] ()
addCol typeName md = \case
  SqelPrim (defaultPrimMeta @tag -> meta) _ ->
    addPrim typeName meta md
  SqelNest (defaultCompMeta @tag -> meta) _ _ _ ->
    addComp typeName meta md
  SqelMerge _ (defaultSort -> compSort) sub _ -> do
    addIndexCol typeName compSort
    addCols typeName sub

addCols ::
  ∀ tag table s .
  SListI s =>
  DefaultMeta tag =>
  PgTypeName table ->
  NP (SqelFor tag) s ->
  Writer [MigrationStatement] ()
addCols typeName =
  htraverse_ (addCol typeName Nothing)

addIndexCol ::
  PgTypeName table ->
  CompSort Def ->
  Writer [MigrationStatement] ()
addIndexCol typeName = \case
  CompSum index -> addPrim typeName index Nothing
  _ -> unit

-- TODO Default value is now Text, can be set with `default`
columnStatements' ::
  ∀ tag table .
  DefaultMeta tag =>
  PgTypeName table ->
  ColumnAction tag ->
  Writer [MigrationStatement] ()
columnStatements' typeName = \case
  AddColumn s _ md ->
    addCol typeName md s
  RemoveColumn (PgColumnName name) ->
    alter_ \ alter attr -> [sql|#{alter} drop #{attr} "##{name}"|]
  RenameColumn (PgColumnName old) (PgColumnName new) ->
    alter_ \ alter attr -> [sql|#{alter} rename #{attr} "##{old}" to "##{new}"|]
  RenameColumnType (PgColumnName old) (PgTypeName new) ->
    alter_ \ alter attr -> [sql|#{alter} alter #{attr} ##{old} set data type ##{new}|]
  where
    alter_ = alterStatement typeName

typeActionStatements ::
  DefaultMeta tag =>
  BuildClause tag CreateType =>
  PgTypeName table ->
  TypeAction tag table ->
  Writer [MigrationStatement] ()
typeActionStatements typeName = \case
  ModifyAction _ cols ->
    traverse_ (columnStatements' typeName) cols
  RenameAction newName@(PgTypeName new) cols -> do
    alterStatement typeName \ alter _ -> [sql|#{alter} rename to "##{new}"|]
    traverse_ (columnStatements' newName) cols
  AddAction comp ->
    tell [MigrationStatement () mempty (statementSql (Statement.createType comp))]

typeStatements ::
  DefaultMeta tag =>
  BuildClause tag CreateType =>
  PgTypeName table ->
  TypeAction tag table ->
  [MigrationStatement]
typeStatements name =
  snd .
  runIdentity .
  runWriterT .
  typeActionStatements name

migrationStatements ::
  DefaultMeta tag =>
  BuildClause tag CreateType =>
  PgTableName ->
  MigrationActions tag m ('Mig old new) ->
  [MigrationStatement]
migrationStatements tableName = \case
  AutoActions {..} ->
    typeStatements tableName table <> (Map.toList types >>= \ (name, actions) -> typeStatements name actions)
  CustomActions _ _ ->
    []

migrationSession :: [MigrationStatement] -> Session ()
migrationSession =
  traverse_ \ (MigrationStatement p enc stmt) -> runUnprepared p (Statement stmt enc unit)

tableStatements ::
  BuildClause tag CreateTable =>
  BuildClause tag CreateType =>
  SqelFor tag table ->
  [Sql]
tableStatements s =
  statementSql (Statement.createTable s) : foldTypes (pure . statementSql . Statement.createType) s

migrationsStatements ::
  DefaultMeta tag =>
  BuildClause tag CreateType =>
  TableDdl tag m table (mig : migs) ->
  [[MigrationStatement]]
migrationsStatements migrations =
  foldMigrations migrations \ Migration {tableFrom, actions} ->
    [migrationStatements (sqelTableName tableFrom) actions]

migrationsTableStatements ::
  BuildClause tag CreateTable =>
  BuildClause tag CreateType =>
  TableDdl tag m table (mig : migs) ->
  [[Sql]]
migrationsTableStatements migrations =
  tableStatements (tableDdlCurrent migrations) :
  foldMigrations migrations \ Migration {tableFrom} -> [tableStatements tableFrom]
