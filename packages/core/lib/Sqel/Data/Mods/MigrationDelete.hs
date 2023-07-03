module Sqel.Data.Mods.MigrationDelete where

import Sqel.Class.ReifyDecoder (DecoderMod)
import Sqel.Class.ReifyEncoder (EncoderMod)
import Sqel.Data.Mods.Sort (ModSort (Skip))

data MigrationDelete

type instance DecoderMod MigrationDelete = 'Skip
type instance EncoderMod MigrationDelete = 'Skip
