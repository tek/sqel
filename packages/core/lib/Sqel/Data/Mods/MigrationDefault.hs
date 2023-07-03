module Sqel.Data.Mods.MigrationDefault where

import Sqel.Class.ReifyDecoder (DecoderMod)
import Sqel.Class.ReifyEncoder (EncoderMod)
import Sqel.Data.Mods.Sort (ModSort (Skip))

type MigrationDefault :: Symbol -> Type
data MigrationDefault value

type instance DecoderMod (MigrationDefault _) = 'Skip
type instance EncoderMod (MigrationDefault _) = 'Skip
