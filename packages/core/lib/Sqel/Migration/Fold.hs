module Sqel.Migration.Fold where

import Sqel.Data.Migration (Mig, Migration, Migrations (InitialMigration, Migrations), TableDdl (TableMigrations))

foldMigrations ::
  ∀ tag m ext table (mig :: Mig ext) (migs :: [Mig ext]) a .
  Semigroup a =>
  TableDdl tag m table (mig : migs) ->
  (∀ (mg :: Mig ext) . Migration tag m mg -> a) ->
  a
foldMigrations (TableMigrations _ migs) f =
  spin migs
  where
    spin :: ∀ (migs' :: [Mig ext]) . Migrations tag m migs' -> a
    spin = \case
      Migrations mig rest -> f mig <> spin rest
      InitialMigration mig -> f mig
