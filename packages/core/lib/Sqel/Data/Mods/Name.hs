module Sqel.Data.Mods.Name where

import Sqel.Class.ReifyDecoder (ReifyDecoder (reifyDecoder))
import Sqel.Class.ReifyEncoder (ReifyEncoder (reifyEncoder))

type SetPrimName :: Symbol -> Type
data SetPrimName name

instance ReifyDecoder mods a => ReifyDecoder (SetPrimName n : mods) a where
  reifyDecoder = reifyDecoder @mods

instance ReifyEncoder mods a => ReifyEncoder (SetPrimName n : mods) a where
  reifyEncoder = reifyEncoder @mods

type SetTableName :: Symbol -> Type
data SetTableName name

instance ReifyDecoder mods a => ReifyDecoder (SetTableName n : mods) a where
  reifyDecoder = reifyDecoder @mods

instance ReifyEncoder mods a => ReifyEncoder (SetTableName n : mods) a where
  reifyEncoder = reifyEncoder @mods
