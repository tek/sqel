module Sqel.Data.Mods.Enum where

import qualified Hasql.Encoders as Encoders
import Prelude hiding (Enum)

import Sqel.Class.ReifyDecoder (ReifyDecoder (reifyDecoder))
import Sqel.Class.ReifyEncoder (ReifyEncoder (reifyEncoder))
import Sqel.Codec.PrimDecoder (enumDecoder)
import Sqel.SOP.Enum (EnumTable)

data Enum = Enum

instance Show a => ReifyEncoder (Enum : mods) a where
  reifyEncoder = Encoders.enum show

instance EnumTable a => ReifyDecoder (Enum : mods) a where
  reifyDecoder = enumDecoder
