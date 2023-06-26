module Sqel.Data.Mods.Unique where

import Sqel.Class.ReifyDecoder (ReifyDecoder (reifyDecoder))
import Sqel.Class.ReifyEncoder (ReifyEncoder (reifyEncoder))

data Unique

instance ReifyDecoder mods a => ReifyDecoder (Unique : mods) a where
  reifyDecoder = reifyDecoder @mods

instance ReifyEncoder mods a => ReifyEncoder (Unique : mods) a where
  reifyEncoder = reifyEncoder @mods
