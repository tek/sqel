module Sqel.Data.Mods.Name where

import Sqel.Class.ReifyDecoder (DecoderMod)
import Sqel.Class.ReifyEncoder (EncoderMod)
import Sqel.Data.Mods.Sort (ModSort (Skip))

type SetPrimName :: Symbol -> Type
data SetPrimName name

type instance DecoderMod (SetPrimName _) = 'Skip
type instance EncoderMod (SetPrimName _) = 'Skip

type SetTableName :: Symbol -> Type
data SetTableName name

type instance DecoderMod (SetTableName _) = 'Skip
type instance EncoderMod (SetTableName _) = 'Skip
