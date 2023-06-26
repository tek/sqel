module Sqel.Data.Mods.MigrationDefault where

import Sqel.Class.ReifyDecoder (ReifyDecoder (reifyDecoder))
import Sqel.Class.ReifyEncoder (ReifyEncoder (reifyEncoder))

type MigrationDefault :: Symbol -> Type
data MigrationDefault value

instance ReifyDecoder mods a => ReifyDecoder (MigrationDefault value : mods) a where
  reifyDecoder = reifyDecoder @mods

instance ReifyEncoder mods a => ReifyEncoder (MigrationDefault value : mods) a where
  reifyEncoder = reifyEncoder @mods
