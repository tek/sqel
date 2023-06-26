module Sqel.Data.Mods.MigrationRename where

import Sqel.Class.ReifyDecoder (ReifyDecoder (reifyDecoder))
import Sqel.Class.ReifyEncoder (ReifyEncoder (reifyEncoder))

type MigrationRename :: Symbol -> Type
data MigrationRename name

instance ReifyDecoder mods a => ReifyDecoder (MigrationRename name : mods) a where
  reifyDecoder = reifyDecoder @mods

instance ReifyEncoder mods a => ReifyEncoder (MigrationRename name : mods) a where
  reifyEncoder = reifyEncoder @mods
