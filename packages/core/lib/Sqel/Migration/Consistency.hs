module Sqel.Migration.Consistency where

import qualified Control.Exception as Base
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT, withExceptT)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty ((<|))
import qualified Data.Map.Strict as Map
import Exon (exon)
import Path (Abs, Dir, File, Path, parseRelFile, toFilePath, (</>))
import Path.IO (createDirIfMissing, doesFileExist)
import System.IO.Error (IOError)

import Sqel.Build.Sql (BuildClause)
import Sqel.Class.DefaultFields (DefaultMeta)
import Sqel.Class.TransformMeta (TransformMeta, transform)
import qualified Sqel.Data.Migration as Migration
import Sqel.Data.Migration (
  Migration (Migration),
  MigrationVersion,
  Migrations (InitialMigration, Migrations),
  TableDdl (TableDdl, TableMigrations),
  latestMigrationVersion,
  )
import Sqel.Data.PgTypeName (getPgTypeName)
import qualified Sqel.Data.Spine
import Sqel.Data.Spine (Spine)
import Sqel.Data.Sqel (SqelFor)
import Sqel.Data.Sql (Sql)
import Sqel.Default (CreateTable, CreateType)
import Sqel.Migration.Consistency.Check (CMC, checkMigrationConsistency)
import qualified Sqel.Migration.Data.MigrationMetadata
import Sqel.Migration.Data.MigrationMetadata (MigrationMetadata (MigrationMetadata))
import Sqel.Migration.Statement (migrationStatementSql, migrationStatements, tableStatements)
import Sqel.Spine (spineTableName, spineTypeCols)
import Sqel.Sqel (sqelTableName, sqelTypes)

tryIO :: MonadIO m => IO a -> m (Either Text a)
tryIO = liftIO . fmap (first show) . Base.try @IOError

tableMetadata ::
  DefaultMeta tag =>
  BuildClause tag CreateTable =>
  BuildClause tag CreateType =>
  MigrationVersion ->
  SqelFor tag table ->
  [Sql] ->
  MigrationMetadata tag
tableMetadata version s statementsMigration =
  MigrationMetadata {
    name = spineTableName types.table,
    version,
    table = spineTypeCols types.table,
    types = typeMap,
    statementsTable = tableStatements s,
    statementsMigration
  }
  where
    typeMap = snd <$> Map.toAscList types.comp
    types = sqelTypes s

migrationMetadata ::
  DefaultMeta tag =>
  BuildClause tag CreateTable =>
  BuildClause tag CreateType =>
  Migration tag m mig ->
  MigrationMetadata tag
migrationMetadata Migration {version, tableFrom, actions} =
  tableMetadata version tableFrom (migrationStatementSql <$> migrationStatements (sqelTableName tableFrom) actions)

migrationMetadatas ::
  DefaultMeta tag =>
  BuildClause tag CreateTable =>
  BuildClause tag CreateType =>
  Migrations tag m migs ->
  NonEmpty (MigrationMetadata tag)
migrationMetadatas = \case
  InitialMigration m -> [migrationMetadata m]
  Migrations m ms -> migrationMetadata m <| migrationMetadatas ms

migrationsMetadata ::
  DefaultMeta tag =>
  BuildClause tag CreateTable =>
  BuildClause tag CreateType =>
  TableDdl tag m table migs ->
  NonEmpty (MigrationMetadata tag)
migrationsMetadata (TableMigrations table migs) =
  NonEmpty.reverse (tableMetadata (latestMigrationVersion migs + 1) table [] <| migrationMetadatas migs)
migrationsMetadata (TableDdl table) =
  [tableMetadata 0 table []]

jsonFile ::
  DefaultMeta tag =>
  Spine tag ->
  String
jsonFile s =
  [exon|##{getPgTypeName (spineTableName s)}.json|]

jsonPath ::
  Monad m =>
  DefaultMeta tag =>
  Path Abs Dir ->
  Spine tag ->
  ExceptT Text m (Path Abs File)
jsonPath dir table = do
  name <- ExceptT (pure (first pathError (parseRelFile (jsonFile table))))
  pure (dir </> name)
  where
    pathError _ = [exon|Table name couldn't be converted to a path: #{toText tname}|]
    tname = jsonFile table

migTable :: TableDdl tag m table migs -> SqelFor tag table
migTable = \case
  TableDdl table -> table
  TableMigrations table _ -> table

migrationMetadataJson ::
  ∀ tag m table migs .
  DefaultMeta tag =>
  TransformMeta tag CMC =>
  BuildClause tag CreateTable =>
  BuildClause tag CreateType =>
  TableDdl tag m table migs ->
  LByteString
migrationMetadataJson migs =
  Aeson.encode (transform @tag @CMC <$> migrationsMetadata migs)

writeMigrationMetadata ::
  ∀ tag m table migs .
  MonadIO m =>
  DefaultMeta tag =>
  TransformMeta tag CMC =>
  BuildClause tag CreateTable =>
  BuildClause tag CreateType =>
  Path Abs Dir ->
  TableDdl tag m table migs ->
  ExceptT Text m ()
writeMigrationMetadata dir migs = do
  path <- jsonPath dir (sqelTypes (migTable migs)).table
  let
    write = LByteString.writeFile (toFilePath path) (migrationMetadataJson migs)
    writeError e = [exon|Couldn't write migration metadata to '#{show path}': #{e}|]
  ExceptT (first writeError <$> tryIO (createDirIfMissing True dir))
  ExceptT (first writeError <$> tryIO write)

readError :: Path Abs File -> Text -> Text
readError path e =
  [exon|Couldn't read migration metadata from #{show path}: #{e}|]

decodeError :: Path Abs File -> String -> Text
decodeError path e =
  [exon|Migration metadata in '#{show path}' has invalid json format: ##{e}|]

readMigrationMetadata ::
  MonadIO m =>
  DefaultMeta tag =>
  Path Abs Dir ->
  TableDdl tag m table migs ->
  ExceptT Text m (Maybe (NonEmpty (MigrationMetadata CMC)))
readMigrationMetadata dir migs = do
  path <- jsonPath dir (sqelTypes (migTable migs)).table
  liftIO (fromRight False <$> tryIO (doesFileExist path)) >>= \case
    False ->
      pure Nothing
    True -> do
      j <- ExceptT (first (readError path) <$> tryIO (ByteString.readFile (toFilePath path)))
      ExceptT (pure (first (decodeError path) (Aeson.eitherDecodeStrict' j)))

single ::
  Functor m =>
  ExceptT Text m a ->
  ExceptT (NonEmpty Text) m a
single =
  withExceptT pure

result ::
  Functor m =>
  ExceptT e m () ->
  m (Maybe e)
result =
  runExceptT >>> fmap \case
    Left e -> Just e
    Right () -> Nothing

migrationConsistency ::
  ∀ tag m table migs .
  MonadIO m =>
  DefaultMeta tag =>
  TransformMeta tag CMC =>
  BuildClause tag CreateTable =>
  BuildClause tag CreateType =>
  Path Abs Dir ->
  TableDdl tag m table migs ->
  Bool ->
  m (Maybe (NonEmpty Text))
migrationConsistency dir migs =
  result . \case
    True ->
      single (writeMigrationMetadata dir migs)
    False ->
      single (readMigrationMetadata dir migs) >>= \case
        Just golden ->
          ExceptT (pure (checkMigrationConsistency golden (transform @tag @CMC <$> migrationsMetadata migs)))
        Nothing -> single (writeMigrationMetadata dir migs)
