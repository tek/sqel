{-# options_ghc -Wno-partial-type-signatures #-}

module Sqel.Test.MigrationRunTest where

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, state)
import Control.Monad.Trans.Writer.Strict (WriterT (runWriterT), tell)
import qualified Data.Map.Strict as Map
import Exon (exon)
import Hasql.Statement (Statement (Statement))
import Hedgehog (TestT, (===))
import Lens.Micro ((^.))
import Prelude hiding (sum)
import Test (unitTest)
import Test.Tasty (TestTree, testGroup)

import Sqel.Class.MigrationEffect (MigrationEffect (..))
import Sqel.Comp (typePrefix)
import Sqel.Data.Dd (Sqel, type (:>) ((:>)))
import Sqel.Data.ExistingColumn (ExistingColumn (ExistingColumn))
import Sqel.Data.Migration (AutoMigrations, Mig (Mig), Migrations, migrate)
import Sqel.Data.Sql (Sql (Sql), unSql)
import Sqel.Data.TableSchema (TableSchema)
import Sqel.Migration.Data.TypeStatus (TypeStatus (Match))
import Sqel.Migration.Dd (migrateRename, migrateRenameType)
import Sqel.Migration.Run (runMigrations)
import Sqel.Migration.Statement (MigrationStatement (MigrationStatement))
import Sqel.Migration.Table (migrateAuto)
import Sqel.Migration.Transform (MigrateTransform, migrateTransform)
import Sqel.Names (typeAs)
import Sqel.PgType (tableSchema)
import Sqel.Prim (array, newtyped, prim, primIndex, primNullable, prims)
import Sqel.Product (prod)
import Sqel.Statement (tableColumnsSql, typeColumnsSql)
import Sqel.Sum (con1, indexPrefix, sum, sumWith)

newtype MockDb m a =
  MockDb { unMockDb :: StateT [Map Text [ExistingColumn]] (WriterT [Text] m) a }
  deriving stock (Generic)
  deriving newtype (Functor, Applicative, Monad)

instance MonadTrans MockDb where
  lift = MockDb . lift . lift

stmt ::
  Monad m =>
  ByteString ->
  MockDb m [a]
stmt c = do
  MockDb (lift (tell [decodeUtf8 c]))
  pure []

takeState ::
  MonadFail m =>
  Text ->
  MockDb m [ExistingColumn]
takeState name =
  MockDb do
    result <- state \case
      [] -> (Nothing, [])
      (cols : colss) -> (Just (fromMaybe mempty (Map.lookup name cols)), colss)
    maybe (fail "Mock columns exhausted") pure result

instance MonadFail m => MigrationEffect (MockDb m) where
  runMigrationStatements =
    traverse_ \ (MigrationStatement _ _ (Sql code)) -> MockDb (lift (tell [code]))
  runStatement _ (Statement code _ _ _) = stmt code
  runStatement_ _ (Statement code _ _ _) = void (stmt code)
  dbCols name (Statement code _ _ _) = do
    _ <- stmt code
    takeState name
  log _ = unit
  error err = lift (fail (toString err))

runMockDb ::
  Monad m =>
  [Map Text [ExistingColumn]] ->
  MockDb m a ->
  m (a, [Text])
runMockDb cols (MockDb ma) =
  runWriterT (evalStateT ma cols)

data Dat1 =
  Dat1 {
    name :: Text
  }
  deriving stock (Eq, Show, Generic)

data Dat =
  Dat {
    num :: Maybe Int,
    name :: Text
  }
  deriving stock (Eq, Show, Generic)

dd_Dat1 :: Sqel Dat1 _
dd_Dat1 =
  typeAs @"Dat" (prod prim)

dd_Dat :: Sqel Dat _
dd_Dat =
  prod (primNullable :> prim)

migrations ::
  Monad m =>
  Migrations (MockDb m) '[ 'Mig Dat1 Dat (MockDb m) (MigrateTransform (MockDb m) Dat1 Dat) ]
migrations =
  migrate (
    migrateTransform dd_Dat1 dd_Dat (pure . fmap \ (Dat1 n) -> Dat Nothing n)
  )

schema :: TableSchema Dat
schema =
  tableSchema dd_Dat

targetLogs :: [Text]
targetLogs =
  [
    unSql tableColumnsSql,
    [exon|create table "dat" ("num" bigint, "name" text not null)|]
  ]

test_migrationTransformAbsent :: TestT IO ()
test_migrationTransformAbsent = do
  (_, logs) <- runMockDb (repeat mempty) do
    runMigrations (schema ^. #pg) migrations
  targetLogs === logs

migrationsExtraColumn :: AutoMigrations (MockDb m) '[Dat1] Dat
migrationsExtraColumn =
  migrate (
    migrateAuto dd_Dat1 dd_Dat
  )

targetLogsExtraColumn :: [Text]
targetLogsExtraColumn =
  [
    unSql tableColumnsSql,
    unSql tableColumnsSql,
    [exon|alter table "dat" add column num bigint|],
    unSql tableColumnsSql
  ]

test_migrationExtraColumn :: TestT IO ()
test_migrationExtraColumn = do
  (_, logs) <- runMockDb cols do
    runMigrations (schema ^. #pg) migrationsExtraColumn
  targetLogsExtraColumn === logs
  where
    cols =
      repeat [
        ("dat", [
          ExistingColumn "name" "text" "" Nothing False,
          ExistingColumn "nonsense" "text" "" Nothing True
        ])
      ]

data Old =
  Old {
    name :: Text,
    status :: Text,
    cats :: [Text]
  }
  deriving stock (Eq, Show, Generic)

dd_Old :: Sqel Old _
dd_Old =
  typeAs @"new" (prod (prim :> prim :> array prim))

newtype Dogs =
  Dogs { unDogs :: [Text] }
  deriving stock (Eq, Show, Generic)

data New =
  New {
    name :: Text,
    status :: Text,
    dogs :: Dogs
  }
  deriving stock (Eq, Show, Generic)

dd_New :: Sqel New _
dd_New =
  prod (prim :> prim :> newtyped (array prim))

schemaNew :: TableSchema New
schemaNew =
  tableSchema dd_New

transform :: Old -> New
transform Old {..} = New {dogs = Dogs cats, ..}

migrationsTransform ::
  Monad m =>
  Migrations (MockDb m) '[ 'Mig Old New (MockDb m) (MigrateTransform (MockDb m) Old New) ]
migrationsTransform =
  migrate (
    migrateTransform dd_Old dd_New (pure . fmap transform)
  )

targetLogsTransform :: [Text]
targetLogsTransform =
  [
    unSql tableColumnsSql,
    unSql tableColumnsSql,
    [exon|select "name", "status", "cats" from "new"|],
    [exon|alter table "new" rename to "new-migration-temp"|],
    [exon|create table "new" ("name" text not null, "status" text not null, "dogs" text[] not null)|],
    unSql tableColumnsSql
  ]

test_migrationTransform :: TestT IO ()
test_migrationTransform = do
  (status, logs) <- runMockDb cols do
    runMigrations (schemaNew ^. #pg) migrationsTransform
  Match === status
  targetLogsTransform === logs
  where
    cols =
      repeat [
        ("new", [
          ExistingColumn "name" "text" "" Nothing False,
          ExistingColumn "status" "text" "" Nothing False,
          ExistingColumn "cats" "ARRAY" "" (Just "text") False
        ])
      ]

data SK =
  SK {
    name :: Text,
    cat :: Text
  }
  deriving stock (Eq, Show, Generic)

data SumKey =
  SK1 { sk1 :: SK }
  |
  SK2 { sk2 :: SK }
  deriving stock (Eq, Show, Generic)

dd_SK :: Sqel SK _
dd_SK =
  prod prims

dd_SumKey_old :: Sqel SumKey _
dd_SumKey_old =
  indexPrefix @"old_sum_index__" (sum (con1 (typePrefix @"old_type__" dd_SK) :> con1 (typePrefix @"old_type__" dd_SK)))

dd_SumKey :: Sqel SumKey _
dd_SumKey =
  sumWith (migrateRename @"old_sum_index__SumKey" (primIndex @"SumKey")) (con1 (migrateRenameType @"old_type__SK" (prod prims)) :> con1 (migrateRenameType @"old_type__SK" (prod prims)))

schemaSumKey :: TableSchema SumKey
schemaSumKey =
  tableSchema dd_SumKey

migrationsSumKey :: AutoMigrations (MockDb m) '[SumKey] SumKey
migrationsSumKey =
  migrate (
    migrateAuto dd_SumKey_old dd_SumKey
  )

targetLogsSumKey1 :: [Text]
targetLogsSumKey1 =
  [
    unSql tableColumnsSql,
    unSql typeColumnsSql,
    unSql tableColumnsSql,
    unSql typeColumnsSql,
    [exon|alter type "old_type__sk" rename to sqel_type__sk|],
    [exon|alter table "sum_key" rename column old_sum_index__sum_key to sqel_sum_index__sum_key|],
    unSql tableColumnsSql,
    unSql typeColumnsSql
  ]

targetLogsSumKey2 :: [Text]
targetLogsSumKey2 =
  [
    unSql tableColumnsSql,
    unSql typeColumnsSql,
    unSql tableColumnsSql,
    unSql typeColumnsSql,
    unSql tableColumnsSql,
    unSql typeColumnsSql
  ]

test_migrationSumKey :: TestT IO ()
test_migrationSumKey = do
  (status1, logs1) <- runMockDb cols1 do
    runMigrations (schemaSumKey ^. #pg) migrationsSumKey
  Match === status1
  targetLogsSumKey1 === logs1
  (status2, logs2) <- runMockDb cols2 do
    runMigrations (schemaSumKey ^. #pg) migrationsSumKey
  Match === status2
  targetLogsSumKey2 === logs2
  where
    cols1 = replicate 5 oldTable ++ [newTable]
    cols2 = repeat newTable
    oldTable =
      [
        ("sum_key", [
          ExistingColumn "old_sum_index__sum_key" "bigint" "" Nothing False,
          ExistingColumn "sk1" "USER-DEFINED" "old_type__sk" Nothing False,
          ExistingColumn "sk2" "USER-DEFINED" "old_type__sk" Nothing False
        ]),
        ("old_type__sk", [
          ExistingColumn "name" "text" "" Nothing False,
          ExistingColumn "cat" "text" "" Nothing False
        ])
      ]
    newTable =
      [
        ("sum_key", [
          ExistingColumn "sqel_sum_index__sum_key" "bigint" "" Nothing False,
          ExistingColumn "sk1" "USER-DEFINED" "sqel_type__sk" Nothing False,
          ExistingColumn "sk2" "USER-DEFINED" "sqel_type__sk" Nothing False
        ]),
        ("sqel_type__sk", [
          ExistingColumn "name" "text" "" Nothing False,
          ExistingColumn "cat" "text" "" Nothing False
        ])
      ]

test_migrations :: TestTree
test_migrations =
  testGroup "migrations" [
    unitTest "transform with absent table" test_migrationTransformAbsent,
    unitTest "extraneous nullable columns" test_migrationExtraColumn,
    unitTest "transform with present table" test_migrationTransform,
    unitTest "migrate twice with sum key rename" test_migrationSumKey
  ]
