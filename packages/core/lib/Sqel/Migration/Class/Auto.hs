module Sqel.Migration.Class.Auto where

import Sqel.Data.Dd (Dd)
import qualified Sqel.Data.Migration as Migration
import Sqel.Data.Migration (Mig (Mig), Migration (Migration), MigrationActions (AutoActions), MigrationVersion)
import Sqel.Data.Sqel (SqelFor)
import Sqel.Migration.Table (TableTypeChanges (tableTypeChanges))
import Sqel.Migration.Type (TableChange (tableChange))

type AutoMigration :: ∀ {ext} . (Type -> Type) -> Type -> Dd ext -> Dd ext -> Constraint
class AutoMigration m tag old new where
  autoMigration :: MigrationVersion -> SqelFor tag old -> SqelFor tag new -> Migration tag m ('Mig old new)

instance (
    TableChange tag old new,
    TableTypeChanges tag old new
  ) => AutoMigration m tag old new where
    autoMigration version old new =
      Migration version old new actions
      where
        actions =
          AutoActions {
            table = tableChange old new,
            types = tableTypeChanges old new
          }
