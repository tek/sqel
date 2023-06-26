module Sqel.Test.Migration.RunTest where

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, state)
import Control.Monad.Trans.Writer.Strict (WriterT (runWriterT), tell)
import qualified Data.Map.Strict as Map
import Exon (exon)
import Hedgehog (TestT, (===))
import Test (unitTest)
import Test.Tasty (TestTree, testGroup)

import Sqel.Class.MigrationEffect (MigrationEffect (..))
import Sqel.Class.ReifySqel (sqel)
import Sqel.Data.ExistingColumn (ExistingColumn (ExistingColumn))
import Sqel.Data.Migration (Mig (Mig), Migrate, TableDdl)
import Sqel.Data.Sqel (sqelSpine)
import Sqel.Data.Sql (Sql (Sql))
import Sqel.Data.Statement (Statement (Statement))
import Sqel.Default (Def, SpineDef, Sqel)
import Sqel.Dsl (
  Con1,
  Gen,
  IndexPrefix,
  Name,
  Newtype,
  Prim,
  Prod,
  RenameIndex,
  RenameType,
  Sum,
  Table,
  TypeName,
  TypePrefix,
  )
import Sqel.Migration.Class.Syntax ((-->))
import Sqel.Migration.Data.TypeStatus (TypeStatus (Match))
import Sqel.Migration.Run (runMigrations)
import Sqel.Migration.Statement (MigrationStatement (MigrationStatement))
import Sqel.Migration.Transform (transform)
import Sqel.Statement.PgSchema (tableColumnsSql, typeColumnsSql)

newtype MockDb m a =
  MockDb { unMockDb :: StateT [Map Text [ExistingColumn]] (WriterT [Text] m) a }
  deriving stock (Generic)
  deriving newtype (Functor, Applicative, Monad)

instance MonadTrans MockDb where
  lift = MockDb . lift . lift

stmt ::
  Monad m =>
  Sql ->
  MockDb m [a]
stmt (Sql c) = do
  MockDb (lift (tell [c]))
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
  runStatement _ (Statement code _ _) = stmt code
  runStatement_ _ (Statement code _ _) = void (stmt code)
  dbCols name (Statement code _ _) = do
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

type Table_Dat1 = Table "dat" Dat1 Gen

table_Dat1 :: Sqel Table_Dat1
table_Dat1 = sqel

data Dat =
  Dat {
    num :: Maybe Int,
    name :: Text
  }
  deriving stock (Eq, Show, Generic)

type Table_Dat = Table "dat" Dat (Prod [Prim :: Type, Prim])
  -- Table (Prod Dat *> Nullable (Prim "num" Int) > Prim "name" Text)

table_Dat :: Sqel Table_Dat
table_Dat = sqel

spine_Dat :: SpineDef
spine_Dat = sqelSpine table_Dat

migrations ::
  ∀ m .
  MonadFail m =>
  TableDdl Def (MockDb m) Table_Dat '[ 'Mig Table_Dat1 Table_Dat]
migrations =
  table_Dat1 --> transform (pure @(MockDb m) . fmap \ (Dat1 n) -> Dat Nothing n) table_Dat

targetLogs :: [Text]
targetLogs =
  coerce @[_] [
    tableColumnsSql,
    [exon|create table "dat" ("num" bigint, "name" text not null)|]
  ]

test_migrationTransformAbsent :: TestT IO ()
test_migrationTransformAbsent = do
  (_, logs) <- runMockDb (repeat mempty) do
    runMigrations migrations
  targetLogs === logs

migrationsExtraColumn ::
  ∀ m .
  Migrate Def (MockDb m) [Table_Dat, Table_Dat1]
migrationsExtraColumn =
  table_Dat1 --> table_Dat

targetLogsExtraColumn :: [Text]
targetLogsExtraColumn =
  coerce @[_] [
    tableColumnsSql,
    tableColumnsSql,
    [exon|alter table "dat" add column "num" bigint|],
    tableColumnsSql
  ]

test_migrationExtraColumn :: TestT IO ()
test_migrationExtraColumn = do
  (_, logs) <- runMockDb cols do
    runMigrations migrationsExtraColumn
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

type Table_Old = Table "new" Old Gen
  -- Table (TName "new" (Prod Old *> Prim "name" Text > Prim "status" Text > PrimArray "cats" Text []))

table_Old :: Sqel Table_Old
table_Old = sqel

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

-- TODO when an Array is used with a type that isn't wrapped in the functor (like manually adding ArrayColumn to a
-- scalar Dd) it is silently accepted.
-- Need to enforce somewhere centrally that the mods are right, probably shouldn't _only_ be done in PrimName.
-- same for Nullable.
type Table_New = Table "new" New (Prod [Prim, Prim, Newtype Prim])
-- (Prod New *> Prim "name" Text > Prim "status" Text > Newtyped Dogs (PrimArray "dogs" Text []))

table_New :: Sqel Table_New
table_New = sqel

transformOld :: Old -> New
transformOld Old {..} = New {dogs = Dogs cats, ..}

migrationsTransform ::
  ∀ m .
  MonadFail m =>
  Migrate Def (MockDb m) [Table_New, Table_Old]
migrationsTransform =
  table_Old --> transform (pure @(MockDb m) . fmap transformOld) table_New

targetLogsTransform :: [Text]
targetLogsTransform =
  coerce @[_] [
    tableColumnsSql,
    tableColumnsSql,
    [exon|select "name", "status", "cats" from "new"|],
    [exon|alter table "new" rename to "new-migration-temp"|],
    [exon|create table "new" ("name" text not null, "status" text not null, "dogs" text[] not null)|],
    tableColumnsSql
  ]

test_migrationTransform :: TestT IO ()
test_migrationTransform = do
  (status, logs) <- runMockDb cols do
    runMigrations migrationsTransform
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

type Table_SumKey_old =
  Table "sum_key" SumKey (IndexPrefix "old_sum_index__" (Sum [
    Con1 (TypePrefix "old_type__" (TypeName "sk" Gen)),
    Con1 (TypePrefix "old_type__" (TypeName "sk" Gen))
    ]))

table_SumKey_old :: Sqel Table_SumKey_old
table_SumKey_old = sqel

type Table_SumKey =
  Table "sum_key" SumKey (RenameIndex "old_sum_index__sum_key" (Sum [
    (Con1 (RenameType "old_type__sk" (TypeName "sk" (Name "sk1" Gen)))),
    (Con1 (RenameType "old_type__sk" (TypeName "sk" (Name "sk2" Gen))))
  ]))

table_SumKey :: Sqel Table_SumKey
table_SumKey = sqel

migrationsSumKey ::
  ∀ m .
  Migrate Def (MockDb m) [Table_SumKey, Table_SumKey_old]
migrationsSumKey =
  table_SumKey_old --> table_SumKey

targetLogsSumKey1 :: [Text]
targetLogsSumKey1 =
  coerce @[_] [
    tableColumnsSql,
    typeColumnsSql,
    tableColumnsSql,
    typeColumnsSql,
    [exon|alter type "old_type__sk" rename to "sqel_type__sk"|],
    [exon|alter table "sum_key" rename column "old_sum_index__sum_key" to "sqel_sum_index__sum_key"|],
    tableColumnsSql,
    typeColumnsSql
  ]

targetLogsSumKey2 :: [Text]
targetLogsSumKey2 =
  coerce @[_] [
    tableColumnsSql,
    typeColumnsSql,
    tableColumnsSql,
    typeColumnsSql,
    tableColumnsSql,
    typeColumnsSql
  ]

test_migrationSumKey :: TestT IO ()
test_migrationSumKey = do
  (status1, logs1) <- runMockDb cols1 do
    runMigrations migrationsSumKey
  Match === status1
  targetLogsSumKey1 === logs1
  (status2, logs2) <- runMockDb cols2 do
    runMigrations migrationsSumKey
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

test_migrationRun :: TestTree
test_migrationRun =
  testGroup "run" [
    unitTest "transform with absent table" test_migrationTransformAbsent,
    unitTest "extraneous nullable columns" test_migrationExtraColumn,
    unitTest "transform with present table" test_migrationTransform,
    unitTest "migrate twice with sum key rename" test_migrationSumKey
  ]
