module Sqel.Class.MigrationEffect where

import Control.Monad.Trans.Writer.Strict (Writer, tell)
import Hasql.Session (Session)

import Sqel.Data.ExistingColumn (ExistingColumn)
import Sqel.Data.Statement (Statement)
import Sqel.Migration.Statement (MigrationStatement, migrationSession)
import Sqel.Statement (runUnprepared)

class MigrationEffect m where
  runMigrationStatements :: [MigrationStatement] -> m ()
  runStatement :: q -> Statement q a -> m [a]
  runStatement_ :: q -> Statement q () -> m ()
  dbCols :: Text -> Statement Text ExistingColumn -> m [ExistingColumn]
  dbCols = runStatement
  log :: Text -> m ()
  error :: Text -> m ()

instance MigrationEffect Session where
  runMigrationStatements = migrationSession
  runStatement = runUnprepared
  runStatement_ = runUnprepared
  log _ = unit
  error _ = unit

instance MigrationEffect (Const [MigrationStatement]) where
  runMigrationStatements = Const
  runStatement _ _ = Const []
  runStatement_ _ _ = Const []
  log _ = Const []
  error _ = Const []

instance MigrationEffect (Writer [MigrationStatement]) where
  runMigrationStatements = tell
  runStatement _ _ = pure []
  runStatement_ _ _ = unit
  log _ = unit
  error _ = unit

instance MigrationEffect Identity where
  runMigrationStatements _ = unit
  runStatement _ _ = pure []
  runStatement_ _ _ = unit
  dbCols _ _ = pure []
  log _ = unit
  error _ = unit
