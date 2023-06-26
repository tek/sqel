module Sqel.Class.ReifyDecoder where

import qualified Hasql.Decoders as Decoders

import Sqel.Codec (PrimColumnOrError (primDecoderOrError))
import Sqel.Error.PrimCodec (ModUnsupported)

class ReifyDecoder mods a where
  reifyDecoder :: Decoders.Value a

instance {-# overlappable #-} (
    TypeError (ModUnsupported "decoder" "Decoder" mod)
  ) => ReifyDecoder (mod : mods) a where
    reifyDecoder = error "unreachable"

instance PrimColumnOrError a => ReifyDecoder '[] a where
  reifyDecoder = primDecoderOrError
