module Sqel.Data.Mods.PrimaryKey where

import Sqel.Class.ReifyDecoder (DecoderMod)
import Sqel.Class.ReifyEncoder (EncoderMod)
import Sqel.Data.Mods.Sort (ModSort (Skip))

data PrimaryKey

type instance DecoderMod PrimaryKey = 'Skip
type instance EncoderMod PrimaryKey = 'Skip
