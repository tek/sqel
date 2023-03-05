{-# options_ghc -Wno-partial-type-signatures #-}

module Sqel.Test.MigrationRunTest where

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks)
import Control.Monad.Trans.Writer.Strict (WriterT (runWriterT), tell)
import qualified Data.Map.Strict as Map
import Exon (exon)
import Hasql.Statement (Statement (Statement))
import Hedgehog (TestT, (===))
import Lens.Micro ((^.))

import Sqel.Class.MigrationEffect (MigrationEffect (..))
import Sqel.Data.Dd (Sqel, type (:>) ((:>)))
import Sqel.Data.ExistingColumn (ExistingColumn (ExistingColumn))
import Sqel.Data.Migration (AutoMigrations, Mig (Mig), Migrations, migrate)
import Sqel.Data.Sql (Sql (Sql), unSql)
import Sqel.Data.TableSchema (TableSchema)
import Sqel.Migration.Run (runMigrations)
import Sqel.Migration.Statement (MigrationStatement (MigrationStatement))
import Sqel.Migration.Table (migrateAuto)
import Sqel.Migration.Transform (MigrateTransform, migrateTransform)
import Sqel.Names (typeAs)
import Sqel.PgType (tableSchema)
import Sqel.Prim (prim, primNullable)
import Sqel.Product (prod)
import Sqel.Statement (tableColumnsSql)

newtype MockDb m a =
  MockDb { unMockDb :: ReaderT (Map Text [ExistingColumn]) (WriterT [Text] m) a }
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

instance MonadFail m => MigrationEffect (MockDb m) where
  runMigrationStatements =
    traverse_ \ (MigrationStatement _ _ (Sql code)) -> MockDb (lift (tell [code]))
  runStatement _ (Statement code _ _ _) = stmt code
  runStatement_ _ (Statement code _ _ _) = void (stmt code)
  dbCols name (Statement code _ _ _) = do
    _ <- stmt code
    fromMaybe mempty <$> MockDb (asks (Map.lookup name))
  log _ = unit
  error err = lift (fail (toString err))

runMockDb :: Map Text [ExistingColumn] -> MockDb m a -> m (a, [Text])
runMockDb cols (MockDb ma) =
  runWriterT (runReaderT ma cols)

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
  ((), logs) <- runMockDb mempty do
    runMigrations (schema ^. #pg) migrations
  targetLogs === logs

migrationsExtraColumn ::
  AutoMigrations (MockDb m) '[Dat1] Dat
migrationsExtraColumn =
  migrate (
    migrateAuto dd_Dat1 dd_Dat
  )

targetLogsExtraColumn :: [Text]
targetLogsExtraColumn =
  [
    unSql tableColumnsSql,
    unSql tableColumnsSql,
    [exon|alter table "dat" add column num bigint|]
  ]

test_migrationExtraColumn :: TestT IO ()
test_migrationExtraColumn = do
  ((), logs) <- runMockDb cols do
    runMigrations (schema ^. #pg) migrationsExtraColumn
  targetLogsExtraColumn === logs
  where
    cols =
      [
        ("dat", [
          ExistingColumn "name" "text" "" Nothing False,
          ExistingColumn "nonsense" "text" "" Nothing True
        ])
      ]
