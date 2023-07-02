module Sqel.Data.Mods.CondOp where

import Sqel.Class.ReifyDecoder (ReifyDecoder (reifyDecoder))
import Sqel.Class.ReifyEncoder (ReifyEncoder (reifyEncoder))

type CondOp :: Symbol -> Type
data CondOp op

instance ReifyDecoder mods a => ReifyDecoder (CondOp op : mods) a where
  reifyDecoder = reifyDecoder @mods

instance ReifyEncoder mods a => ReifyEncoder (CondOp op : mods) a where
  reifyEncoder = reifyEncoder @mods
