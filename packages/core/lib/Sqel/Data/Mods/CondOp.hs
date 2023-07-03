module Sqel.Data.Mods.CondOp where

import Sqel.Class.ReifyDecoder (DecoderMod)
import Sqel.Class.ReifyEncoder (EncoderMod)
import Sqel.Data.Mods.Sort (ModSort (Skip))

type CondOp :: Symbol -> Type
data CondOp op

type instance DecoderMod (CondOp _) = 'Skip
type instance EncoderMod (CondOp _) = 'Skip
