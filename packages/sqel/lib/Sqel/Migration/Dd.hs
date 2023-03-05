module Sqel.Migration.Dd where

import Sqel.Class.Mods (MapMod, setMod)
import Sqel.Data.Dd (Dd, DdType)
import Sqel.Data.MigrationParams (
  MigrationDefault (MigrationDefault),
  MigrationDelete (MigrationDelete),
  MigrationRename (MigrationRename),
  MigrationRenameType (MigrationRenameType),
  )

migrateDef ::
  ∀ s0 s1 .
  MapMod (MigrationDefault (DdType s0)) s0 s1 =>
  DdType s0 ->
  Dd s0 ->
  Dd s1
migrateDef a =
  setMod (MigrationDefault a)

migrateRename ::
  ∀ name s0 s1 .
  MapMod (MigrationRename name) s0 s1 =>
  Dd s0 ->
  Dd s1
migrateRename =
  setMod (MigrationRename @name)

migrateRenameType ::
  ∀ name s0 s1 .
  MapMod (MigrationRenameType name) s0 s1 =>
  Dd s0 ->
  Dd s1
migrateRenameType =
  setMod (MigrationRenameType @name)

migrateDelete ::
  ∀ s0 s1 .
  MapMod MigrationDelete s0 s1 =>
  Dd s0 ->
  Dd s1
migrateDelete =
  setMod MigrationDelete
