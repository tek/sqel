module Sqel.Data.Mods.Ignore where

import Sqel.Class.ReifyPrimCodec (ReifyPrimCodec (reifyPrimCodec))
import Sqel.Data.Codec (Decoder (Decoder), Encoder (Encoder))

data Ignore

instance ReifyPrimCodec (Ignore : mods) Decoder a where
  reifyPrimCodec =
    Decoder (fail err) (fail err)
    where
      err = "ignored column was used"

instance ReifyPrimCodec (Ignore : mods) Encoder a where
  reifyPrimCodec = Encoder mempty mempty
