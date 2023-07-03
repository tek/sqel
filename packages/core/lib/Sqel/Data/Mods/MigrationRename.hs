module Sqel.Data.Mods.MigrationRename where

import Sqel.Class.ReifyDecoder (DecoderMod)
import Sqel.Class.ReifyEncoder (EncoderMod)
import Sqel.Data.Mods.Sort (ModSort (Skip))

type MigrationRename :: Symbol -> Type
data MigrationRename name

type instance DecoderMod (MigrationRename _) = 'Skip
type instance EncoderMod (MigrationRename _) = 'Skip
