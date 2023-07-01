module Sqel.Migration.Run where

import Control.Monad (foldM)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Exon (exon)
import Prettyprinter (pretty)

import Sqel.Build.Sql (BuildClause)
import Sqel.Class.DefaultFields (DefaultMeta, defaultTypes)
import qualified Sqel.Class.MigrationEffect as MigrationEffect
import Sqel.Class.MigrationEffect (MigrationEffect (runMigrationStatements))
import Sqel.Data.Migration (
  CompAction,
  Mig,
  MigFrom,
  MigTo,
  Migration (Migration),
  MigrationActions (AutoActions, CustomActions),
  MigrationVersion (MigrationVersion),
  Migrations (InitialMigration, Migrations),
  TableDdl (TableDdl, TableMigrations),
  TypeAction (AddAction),
  )
import Sqel.Data.PgType (PgTypeRef (PgTypeRef))
import Sqel.Data.PgTypeName (
  PgCompName,
  pattern PgOnlyTableName,
  PgTableName,
  pattern PgTypeName,
  PgTypeName,
  getPgTypeName,
  pgCompName,
  )
import qualified Sqel.Data.Spine
import Sqel.Data.Spine (Spine, Types)
import Sqel.Data.Sqel (SqelFor, sqelSpine)
import Sqel.Data.Sql (Sql)
import Sqel.Default (CreateType)
import Sqel.Migration.Data.TypeStatus (TypeStatus (Absent, Match, Mismatch))
import Sqel.Migration.Init (InitTable, initTable)
import Sqel.Migration.Metadata (columnMap, logType, typeColumns, typeStatus)
import Sqel.Migration.Statement (typeStatements)
import Sqel.Spine (spineTableName, spineTypeCols, spineTypes)
import Sqel.Sqel (sqelTableName, sqelTypes)
import Sqel.Statement.PgSchema (tableColumnsSql, typeColumnsSql)

typeMatchWith ::
  ∀ tag table m .
  Monad m =>
  DefaultMeta tag =>
  MigrationEffect m =>
  PgTypeName table ->
  [Spine tag] ->
  Sql ->
  m TypeStatus
typeMatchWith name cols code = do
  dbCols <- typeColumns code name
  let status = typeStatus dbCols targetCols
  logType name status dbCols targetCols
  pure status
  where
    targetCols = columnMap cols

typeMatch ::
  Monad m =>
  DefaultMeta tag =>
  MigrationEffect m =>
  PgTypeName table ->
  [Spine tag] ->
  m TypeStatus
typeMatch name cols =
  typeMatchWith name cols typeColumnsSql

tableMatch ::
  ∀ tag m .
  Monad m =>
  DefaultMeta tag =>
  MigrationEffect m =>
  TypeStatus ->
  Spine tag ->
  m TypeStatus
tableMatch Absent _ =
  pure Absent
tableMatch _ s =
  typeMatchWith (spineTableName s) (spineTypeCols s) tableColumnsSql

matches ::
  ∀ tag m .
  Monad m =>
  DefaultMeta tag =>
  MigrationEffect m =>
  TypeStatus ->
  Types tag ->
  m (TypeStatus, Set PgCompName)
matches initialStatus (defaultTypes @tag -> types) = do
  tbm <- tableMatch initialStatus types.table
  tym <- foldM folder Set.empty types.comp
  pure (tbm, tym)
  where
    folder acc comp =
      typeMatch comp.name comp.sub <&> \case
        Match -> Set.insert comp.name acc
        _ -> acc

runAction ::
  DefaultMeta tag =>
  MigrationEffect m =>
  BuildClause tag CreateType =>
  PgTypeName table ->
  TypeAction tag table ->
  m ()
runAction typeName action =
  runMigrationStatements (typeStatements typeName action)

-- TODO topo sort the types
runTypesMigration ::
  Monad m =>
  DefaultMeta tag =>
  MigrationEffect m =>
  BuildClause tag CreateType =>
  Set PgCompName ->
  Map PgCompName (CompAction tag) ->
  m ()
runTypesMigration eligible actions =
  for_ (Map.toList (Map.restrictKeys actions eligible)) \ (name, tpe) ->
    runAction name tpe

runMigration ::
  ∀ tag mig m .
  Monad m =>
  DefaultMeta tag =>
  MigrationEffect m =>
  BuildClause tag CreateType =>
  TypeStatus ->
  PgTableName ->
  SqelFor tag (MigFrom mig) ->
  SqelFor tag (MigTo mig) ->
  Set PgCompName ->
  MigrationActions tag m mig ->
  m ()
runMigration status tableName current new eligible = \case
  AutoActions tableAction typeActions -> do
    MigrationEffect.log [exon|Starting migration for #{getPgTypeName tableName}|]
    runTypesMigration eligible typeActions
    when (status == Match) (runAction tableName tableAction)
  CustomActions _ migration ->
    migration status current new eligible

autoKeys ::
  Map PgCompName (CompAction tag) ->
  Set (PgCompName, Bool)
autoKeys typeActions =
  Set.fromList (Map.elems (Map.mapWithKey keyAndAddition typeActions))
  where
    keyAndAddition k = \case
      AddAction _ -> (k, True)
      _ -> (k, False)

typeKeys ::
  ∀ tag m ext (mig :: Mig ext) .
  Applicative m =>
  MigrationActions tag m mig ->
  m (Set (PgCompName, Bool))
typeKeys = \case
  AutoActions _ typeActions ->
    pure (autoKeys typeActions)
  CustomActions f _ ->
    f

collectDirectMatches :: Set (PgCompName, Bool) -> Set PgCompName -> Set PgCompName
collectDirectMatches actions curMatches =
  Set.fromList (fst <$> filter (uncurry matchAction) (Set.toList actions))
  where
    matchAction name = \case
      True -> not (Set.member name curMatches)
      False -> Set.member name curMatches

matchMessage ::
  PgTypeName table ->
  MigrationVersion ->
  TypeStatus ->
  Set PgCompName ->
  Set PgCompName ->
  Set PgCompName ->
  Text
matchMessage (PgTypeName tableName) (MigrationVersion version) = msg
  where
    msg Absent _ _ _ =
      [exon|#{herald}: Absent|]
    msg status currentMatches directMatches allMatches =
      [exon|#{herald}: #{show (pretty status)}
Matching types: #{showNames currentMatches}
Direct action matches: #{showNames directMatches}
All action matches: #{showNames allMatches}
|]
    herald = [exon|Table '#{tableName}' [#{show version}]:|]
    showNames =
      Text.intercalate ", " .
      fmap (\ (PgTypeName name) -> name) .
      Set.toList

runMigrationStep ::
  ∀ tag m ext (mig :: Mig ext) .
  Monad m =>
  DefaultMeta tag =>
  MigrationEffect m =>
  BuildClause tag CreateType =>
  Migration tag m mig ->
  TypeStatus ->
  Set PgCompName ->
  Types tag ->
  (TypeStatus -> Set PgCompName -> Types tag -> m (TypeStatus, Set PgCompName)) ->
  m (TypeStatus, Set PgCompName)
runMigrationStep (Migration version currentTable newTable actions) laterStatus laterMatches types next = do
  -- types that are identical in the database and the current migration's from-table
  (status, currentTypeMatches) <- matches laterStatus currentTypes
  actionNamesAndAdditions <- typeKeys @tag @m @ext @mig actions
  let
    actionNames = Set.fromList (fst <$> Set.toList actionNamesAndAdditions)
    mismatchHere = case status of
      Mismatch _ -> True
      _ -> False
    -- actions whose types match the database before any migrations are executed.
    directMatches = collectDirectMatches actionNamesAndAdditions currentTypeMatches
    -- actions whose types either match this migration's from-table or that of a later migration.
    allMatches = Set.union directMatches laterMatches
  MigrationEffect.log (matchMessage (spineTableName currentTypes.table) version status currentTypeMatches directMatches allMatches)
  (newStatus, eligible) <-
    -- if actionNames is a subset of allMatches, all actions can be executed either here or in a later migration.
    -- therefore we don't need to check earlier migrations and just execute the direct matches here and relay the rest
    -- to later migrations.
    -- if the current migration's table doesn't match the existing table, we still have to run earlier migrations,
    -- but those don't have to run any type actions.
    -- if the table is absent, earlier migrations don't have to be run, just like a match, but types are still eligible.
    if not mismatchHere && Set.isSubsetOf actionNames allMatches
    then pure (status, directMatches)
    else do
      -- if the table matched in an earlier migration, it will match here as well since the earlier migration was
      -- executed.
      -- same for types, so add earlier matches to the direct matches.
      (earlierStatus, earlierEligible) <- next status allMatches types
      pure (earlierStatus, Set.union earlierEligible directMatches)
  runMigration @tag @mig newStatus (spineTableName types.table) currentTable newTable eligible actions
  let
    -- If this migration or the earlier one mismatched but the later one matched, the new status for the later migration
    -- should be Match.
    updateStatus Absent = const Absent
    updateStatus Match = const Match
    updateStatus (Mismatch _) = id
  pure (updateStatus laterStatus newStatus, eligible)
  where
    currentTypes = sqelTypes currentTable

runMigrationSteps ::
  ∀ tag m migs .
  Monad m =>
  DefaultMeta tag =>
  MigrationEffect m =>
  BuildClause tag CreateType =>
  Migrations tag m migs ->
  TypeStatus ->
  Set PgCompName ->
  Types tag ->
  m (TypeStatus, Set PgCompName)
runMigrationSteps (Migrations mig migs) s m t =
  runMigrationStep mig s m t (runMigrationSteps migs)
runMigrationSteps (InitialMigration mig) s m t =
  runMigrationStep mig s m t \ s' _ _ -> pure (s', mempty)

tableTypes :: Types tag -> Set PgCompName
tableTypes types =
  Set.fromList $ Map.keys types.comp <&> \ (PgTypeRef n) -> pgCompName n

conclusion ::
  Monad m =>
  DefaultMeta tag =>
  MigrationEffect m =>
  InitTable tag table =>
  SqelFor tag table ->
  Types tag ->
  TypeStatus ->
  m ()
conclusion table types = \case
  Absent -> do
    initTable table
    MigrationEffect.log [exon|Finished migrations for '#{name}' by creating the table|]
  s@(Mismatch _) ->
    MigrationEffect.error [exon|Failed to migrate the table '#{name}' due to #{show (pretty s)}|]
  Match -> do
    (_, newTypes) <- matches Match types
    let missing = getPgTypeName <$> Set.toList (Set.difference (tableTypes types) newTypes)
    unless (null missing) do
      MigrationEffect.error [exon|Migration of types failed for table '#{name}': #{Text.intercalate ", " missing}|]
    MigrationEffect.log [exon|Performed migrations for '#{name}' successfully|]
  where
    name = getPgTypeName (sqelTableName table)

runMigrations ::
  ∀ tag m table migs .
  Monad m =>
  DefaultMeta tag =>
  MigrationEffect m =>
  BuildClause tag CreateType =>
  InitTable tag table =>
  TableDdl tag m table migs ->
  m TypeStatus
runMigrations (TableMigrations table steps) = do
  MigrationEffect.log [exon|Checking migrations for '#{name}'|]
  (initialStatus, initialTypes) <- matches Match types
  (status, _) <- runMigrationSteps steps initialStatus initialTypes types
  status <$ conclusion table types status
  where
    PgOnlyTableName name = spineTableName types.table
    types = spineTypes spine
    spine = sqelSpine table
runMigrations (TableDdl table) = do
  MigrationEffect.log [exon|Checking table '#{name}'|]
  (status, _) <- matches Match types
  status <$ conclusion table types status
  where
    PgOnlyTableName name = spineTableName types.table
    types = spineTypes spine
    spine = sqelSpine table
