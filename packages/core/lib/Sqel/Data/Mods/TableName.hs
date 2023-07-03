module Sqel.Data.Mods.TableName where

import Sqel.Class.ReifyDecoder (DecoderMod)
import Sqel.Class.ReifyEncoder (EncoderMod)
import Sqel.Data.Mods.Sort (ModSort (Skip))

type TableName :: Symbol -> Type
data TableName name

type instance DecoderMod (TableName _) = 'Skip
type instance EncoderMod (TableName _) = 'Skip
