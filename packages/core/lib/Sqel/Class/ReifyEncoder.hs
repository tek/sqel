module Sqel.Class.ReifyEncoder where

import qualified Hasql.Encoders as Encoders

import Sqel.Codec (PrimColumnOrError (primEncoderOrError))
import Sqel.Error.PrimCodec (ModUnsupported)

class ReifyEncoder mods a where
  reifyEncoder :: Encoders.Value a

instance {-# overlappable #-} (
    TypeError (ModUnsupported "encoder" "Encoder" mod)
  ) => ReifyEncoder (mod : mods) a where
    reifyEncoder = error "unreachable"

instance (
    PrimColumnOrError a
  ) => ReifyEncoder '[] a where
    reifyEncoder = primEncoderOrError
