module Sqel.Class.MigrationEffect where

import qualified Hasql.Session as Session
import Hasql.Session (Session)
import Hasql.Statement (Statement)

import Sqel.Data.ExistingColumn (ExistingColumn)
import Sqel.Migration.Statement (MigrationStatement, migrationSession)

class MigrationEffect m where
  runMigrationStatements :: [MigrationStatement] -> m ()
  runStatement :: q -> Statement q [a] -> m [a]
  runStatement_ :: q -> Statement q () -> m ()
  dbCols :: Text -> Statement Text [ExistingColumn] -> m [ExistingColumn]
  dbCols = runStatement
  log :: Text -> m ()
  error :: Text -> m ()

instance MigrationEffect Session where
  runMigrationStatements = migrationSession
  runStatement_ = Session.statement
  runStatement = Session.statement
  log _ = unit
  error _ = unit

instance MigrationEffect (Const [MigrationStatement]) where
  runMigrationStatements = Const
  runStatement_ _ _ = Const []
  runStatement _ _ = Const []
  log _ = Const []
  error _ = Const []
