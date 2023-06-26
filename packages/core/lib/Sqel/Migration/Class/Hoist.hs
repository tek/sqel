module Sqel.Migration.Class.Hoist where

import Sqel.Data.Migration (
  Migration (..),
  MigrationActions (AutoActions, CustomActions, action, table, typeKeys, types),
  Migrations (..),
  TableDdl (..),
  )

hoistMigration ::
  ∀ tag m n mig .
  (∀ x . m x -> n x) ->
  Migration tag m mig ->
  Migration tag n mig
hoistMigration f Migration {..} =
  Migration {actions = hoistAction actions, ..}
  where
    hoistAction = \case
      CustomActions {..} -> CustomActions {typeKeys = f typeKeys, action = \ s old new n -> f (action s old new n)}
      AutoActions {..} -> AutoActions {..}

hoistMigrations :: (∀ x . m x -> n x) -> Migrations tag m migs -> Migrations tag n migs
hoistMigrations f (InitialMigration mig) = InitialMigration (hoistMigration f mig)
hoistMigrations f (Migrations mig migs) =
  Migrations (hoistMigration f mig) (hoistMigrations f migs)

hoistTableDdl ::
  (∀ x . m x -> n x) ->
  TableDdl tag m table migs ->
  TableDdl tag n table migs
hoistTableDdl f = \case
  TableDdl t -> TableDdl t
  TableMigrations table migs -> TableMigrations table (hoistMigrations f migs)
