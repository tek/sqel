module Sqel.Data.Mods.MigrationDelete where

import Sqel.Class.ReifyDecoder (ReifyDecoder (reifyDecoder))
import Sqel.Class.ReifyEncoder (ReifyEncoder (reifyEncoder))

data MigrationDelete

instance ReifyDecoder mods a => ReifyDecoder (MigrationDelete : mods) a where
  reifyDecoder = reifyDecoder @mods

instance ReifyEncoder mods a => ReifyEncoder (MigrationDelete : mods) a where
  reifyEncoder = reifyEncoder @mods
