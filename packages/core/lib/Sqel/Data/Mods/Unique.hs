module Sqel.Data.Mods.Unique where

import Sqel.Class.ReifyDecoder (DecoderMod)
import Sqel.Class.ReifyEncoder (EncoderMod)
import Sqel.Data.Mods.Sort (ModSort (Skip))

data Unique

type instance DecoderMod Unique = 'Skip
type instance EncoderMod Unique = 'Skip
