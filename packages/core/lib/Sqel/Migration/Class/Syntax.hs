module Sqel.Migration.Class.Syntax where

import Sqel.Data.Dd (DdK)
import Sqel.Data.Migration (
  Mig (Mig),
  Migration,
  MigrationVersion,
  Migrations (InitialMigration, Migrations),
  TableDdl (TableMigrations),
  latestMigrationVersion,
  )
import Sqel.Data.Sqel (SqelFor)
import Sqel.Dd (DdType)
import Sqel.Migration.Class.Auto (AutoMigration (autoMigration))
import qualified Sqel.Migration.Data.TransformStep
import Sqel.Migration.Data.TransformStep (TransformStep (TransformStep))
import Sqel.Migration.Transform (MkMigrateTransform (migrateTransform))

type MigrationFrom :: Type -> (Type -> Type) -> Type -> DdK ext -> DdK ext -> [Mig ext] -> Constraint
class MigrationFrom tag m left old new migs | tag m left new -> old migs, tag m old migs -> left where
  migrationFrom ::
    Migration tag m ('Mig old new) ->
    left ->
    (SqelFor tag old, Migrations tag m ('Mig old new : migs), MigrationVersion)

instance MigrationFrom tag m (SqelFor tag old) old new '[] where
  migrationFrom mig old =
    (old, InitialMigration mig, 0)

instance MigrationFrom tag m (TableDdl tag m old ('Mig from old : migs)) old new ('Mig from old : migs) where
  migrationFrom mig (TableMigrations old migs) =
    (old, Migrations mig migs, latestMigrationVersion migs + 1)

type MigrationTo :: Type -> (Type -> Type) -> Type -> DdK ext -> DdK ext -> Constraint
class MigrationTo tag m right old new | right -> new where
  migrationTo :: MigrationVersion -> SqelFor tag old -> right -> (SqelFor tag new, Migration tag m ('Mig old new))

instance (
    MkMigrateTransform tag m old new,
    oldT ~ DdType old
  ) => MigrationTo tag m (TransformStep tag m oldT new) old new where
    migrationTo version old TransformStep {..} =
      (new, migrateTransform version old new trans)

instance (
    AutoMigration m tag old new
  ) => MigrationTo tag m (SqelFor tag new) old new where
    migrationTo version old new =
      (new, autoMigration version old new)

(-->) ::
  MigrationTo tag m right old new =>
  MigrationFrom tag m left old new migs =>
  left ->
  right ->
  TableDdl tag m new ('Mig old new : migs)
left --> right =
  TableMigrations new migs
  where
    (old, migs, version) = migrationFrom mig left
    (new, mig) = migrationTo version old right

infixl 5 -->
