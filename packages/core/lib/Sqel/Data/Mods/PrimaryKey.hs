module Sqel.Data.Mods.PrimaryKey where

import Sqel.Class.ReifyDecoder (ReifyDecoder (reifyDecoder))
import Sqel.Class.ReifyEncoder (ReifyEncoder (reifyEncoder))

data PrimaryKey

instance ReifyDecoder mods a => ReifyDecoder (PrimaryKey : mods) a where
  reifyDecoder = reifyDecoder @mods

instance ReifyEncoder mods a => ReifyEncoder (PrimaryKey : mods) a where
  reifyEncoder = reifyEncoder @mods
