module Sqel.Class.ReifyPrimCodec where

import qualified Hasql.Decoders as Decoders
import Hasql.Decoders (Row)
import qualified Hasql.Encoders as Encoders
import Hasql.Encoders (Params)

import Sqel.Class.CompleteCodec (CompleteCodec (completeCodec))
import Sqel.Class.ReifyDecoder (ReifyDecoder (reifyDecoder))
import Sqel.Class.ReifyEncoder (ReifyEncoder, reifyEncoder)
import Sqel.Data.Codec (Codec (Codec), Decoder, Encoder, FullCodec)

type ReifyPrimCodec :: [Type] -> (Type -> Type) -> Type -> Constraint
class ReifyPrimCodec mods b a where
  reifyPrimCodec :: b a

instance {-# overlappable #-} (
    ReifyDecoder mods a
  ) => ReifyPrimCodec mods Decoders.Value a where
    reifyPrimCodec = reifyDecoder @mods

instance {-# overlappable #-} (
    ReifyEncoder mods a
  ) => ReifyPrimCodec mods Encoders.Value a where
    reifyPrimCodec = reifyEncoder @mods

instance {-# overlappable #-} (
    ReifyDecoder mods a
  ) => ReifyPrimCodec mods Row a where
    reifyPrimCodec = completeCodec (reifyDecoder @mods)

instance {-# overlappable #-} (
    ReifyEncoder mods a
  ) => ReifyPrimCodec mods Params a where
    reifyPrimCodec = completeCodec (reifyEncoder @mods)

instance {-# overlappable #-} (
    ReifyDecoder mods a
  ) => ReifyPrimCodec mods Decoder a where
    reifyPrimCodec = completeCodec (reifyDecoder @mods)

instance {-# overlappable #-} (
    ReifyEncoder mods a
  ) => ReifyPrimCodec mods Encoder a where
    reifyPrimCodec = completeCodec (reifyEncoder @mods)

instance {-# overlappable #-} (
    ReifyPrimCodec mods Encoder a,
    ReifyPrimCodec mods Decoder a
  ) => ReifyPrimCodec mods FullCodec a where
    reifyPrimCodec = Codec (reifyPrimCodec @mods) (reifyPrimCodec @mods)
