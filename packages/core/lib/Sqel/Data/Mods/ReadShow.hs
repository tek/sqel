module Sqel.Data.Mods.ReadShow where

import qualified Hasql.Encoders as Encoders

import Sqel.Class.ReifyDecoder (ReifyDecoder (reifyDecoder))
import Sqel.Class.ReifyEncoder (ReifyEncoder (reifyEncoder))
import Sqel.Codec.PrimDecoder (readDecoder)

data ReadShow

instance Show a => ReifyEncoder (ReadShow : mods) a where
  reifyEncoder = Encoders.enum show

instance Read a => ReifyDecoder (ReadShow : mods) a where
  reifyDecoder = readDecoder
