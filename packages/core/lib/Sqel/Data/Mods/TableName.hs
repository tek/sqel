module Sqel.Data.Mods.TableName where

import Sqel.Class.ReifyDecoder (ReifyDecoder (reifyDecoder))
import Sqel.Class.ReifyEncoder (ReifyEncoder (reifyEncoder))

type TableName :: Symbol -> Type
data TableName name

instance ReifyDecoder mods a => ReifyDecoder (TableName name : mods) a where
  reifyDecoder = reifyDecoder @mods

instance ReifyEncoder mods a => ReifyEncoder (TableName name : mods) a where
  reifyEncoder = reifyEncoder @mods
