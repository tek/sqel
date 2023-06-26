module Sqel.Data.Mods (
  module Sqel.Data.Mods,
  module Sqel.Data.Mods.Nullable,
  module Sqel.Data.Mods.Unique,
  module Sqel.Data.Mods.PrimaryKey,
  module Sqel.Data.Mods.Array,
  module Sqel.Data.Mods.Enum,
  module Sqel.Data.Mods.ReadShow,
  module Sqel.Data.Mods.Newtype,
  module Sqel.Data.Mods.Ignore,
  module Sqel.Data.Mods.Name,
  module Sqel.Data.Mods.MigrationDefault,
  module Sqel.Data.Mods.MigrationDelete,
  module Sqel.Data.Mods.MigrationRename,
  module Sqel.Data.Mods.MigrationRenameType,
  module Sqel.Data.Mods.CustomCodec,
) where

import Sqel.Data.Mods.Array (Array)
import Sqel.Data.Mods.CustomCodec (CustomCodec, CustomDecoder (customDecoder), CustomEncoder (customEncoder))
import Sqel.Data.Mods.Enum (Enum)
import Sqel.Data.Mods.Ignore (Ignore)
import Sqel.Data.Mods.MigrationDefault (MigrationDefault)
import Sqel.Data.Mods.MigrationDelete (MigrationDelete)
import Sqel.Data.Mods.MigrationRename (MigrationRename)
import Sqel.Data.Mods.MigrationRenameType (MigrationRenameType)
import Sqel.Data.Mods.Name (SetPrimName, SetTableName)
import Sqel.Data.Mods.Newtype (Newtype)
import Sqel.Data.Mods.Nullable (Nullable)
import Sqel.Data.Mods.PrimaryKey (PrimaryKey)
import Sqel.Data.Mods.ReadShow (ReadShow)
import Sqel.Data.Mods.Unique (Unique)

type NoMods :: [Type]
type NoMods = '[]

type ColumnConstraint :: Symbol -> Type
data ColumnConstraint name = ColumnConstraint
