module Sqel.Data.Mods.MigrationRenameType where

import Sqel.Class.ReifyDecoder (ReifyDecoder (reifyDecoder))
import Sqel.Class.ReifyEncoder (ReifyEncoder (reifyEncoder))

type MigrationRenameType :: Symbol -> Type
data MigrationRenameType name

instance ReifyDecoder mods a => ReifyDecoder (MigrationRenameType name : mods) a where
  reifyDecoder = reifyDecoder @mods

instance ReifyEncoder mods a => ReifyEncoder (MigrationRenameType name : mods) a where
  reifyEncoder = reifyEncoder @mods
