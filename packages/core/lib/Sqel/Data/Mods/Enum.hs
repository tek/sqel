module Sqel.Data.Mods.Enum where

import qualified Hasql.Encoders as Encoders
import Prelude hiding (Enum)

import Sqel.Class.ReifyDecoder (CreateDecoder (createDecoder), DecoderMod)
import Sqel.Class.ReifyEncoder (CreateEncoder (createEncoder), EncoderMod)
import Sqel.Codec.PrimDecoder (enumDecoder)
import Sqel.Data.Mods.Sort (ModSort (Create))
import Sqel.SOP.Enum (EnumTable)

data Enum = Enum

type instance DecoderMod Enum = 'Create
type instance EncoderMod Enum = 'Create

instance Show a => CreateEncoder error Enum a where
  createEncoder = Encoders.enum show

instance EnumTable a => CreateDecoder error Enum a where
  createDecoder = enumDecoder
