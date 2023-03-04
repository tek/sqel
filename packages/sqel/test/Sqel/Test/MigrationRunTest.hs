{-# options_ghc -Wno-partial-type-signatures #-}

module Sqel.Test.MigrationRunTest where

import Control.Monad.Trans.Writer.Strict (WriterT (runWriterT), tell)
import Exon (exon)
import Hasql.Statement (Statement (Statement))
import Hedgehog (TestT, (===))
import Lens.Micro ((^.))
import Prelude hiding (sum)

import Sqel.Class.MigrationEffect (MigrationEffect (..))
import Sqel.Data.Dd (Sqel, type (:>) ((:>)))
import Sqel.Data.Migration (Mig (Mig), Migrations, migrate)
import Sqel.Data.TableSchema (TableSchema)
import Sqel.Migration.Run (runMigrations)
import Sqel.Migration.Transform (MigrateTransform, migrateTransform)
import Sqel.Names (typeAs)
import Sqel.PgType (tableSchema)
import Sqel.Prim (prim, primNullable)
import Sqel.Product (prod)

newtype MockDb m a =
  MockDb { unMockDb :: WriterT [Text] m a }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Functor, Applicative, Monad)

stmt ::
  Monad m =>
  ByteString ->
  MockDb m [a]
stmt c = do
  MockDb (tell [decodeUtf8 c])
  pure []

instance Monad m => MigrationEffect (MockDb m) where
  runMigrationStatements _ = unit
  runStatement _ (Statement code _ _ _) = stmt code
  runStatement_ _ (Statement code _ _ _) = void (stmt code)
  log _ = unit
  error _ = unit

runMockDb :: MockDb m a -> m (a, [Text])
runMockDb (MockDb ma) =
  runWriterT ma

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
    [exon|select c.column_name, c.data_type, c. udt_name, e.data_type from information_schema.columns c left join information_schema.element_types e on ((c.table_catalog, c.table_schema, c.table_name, 'TABLE', c.dtd_identifier) = (e.object_catalog, e.object_schema, e.object_name, e.object_type, e.collection_type_identifier)) where c.table_name = $1|],
    [exon|create table "dat" ("num" bigint, "name" text not null)|]
  ]

test_migrationTransformAbsent :: TestT IO ()
test_migrationTransformAbsent = do
  ((), logs) <- runMockDb do
    runMigrations (schema ^. #pg) migrations
  targetLogs === logs
