module Sqel.Data.Mods.Nullable where

import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders

import Sqel.Class.ReifyPrimCodec (ReifyPrimCodec (reifyPrimCodec))
import Sqel.Codec (columnDecoderNullable, columnEncoderNullable, ignoreDecoder, ignoreEncoder)
import Sqel.Data.Codec (Decoder (Decoder), Encoder (Encoder))

-- | The flag controls whether the column should be considered a match in a query when it is null, irrespective
-- of the query value.
-- This only has an effect when set on the query type.
type Nullable :: Bool -> Type
data Nullable guardQuery

instance (
    ReifyPrimCodec mods Decoders.Value a
  ) => ReifyPrimCodec (Nullable grd : mods) Decoder (Maybe a) where
    reifyPrimCodec =
        Decoder (columnDecoderNullable decoder) (void ignoreDecoder)
      where
        decoder = reifyPrimCodec @mods @Decoders.Value

-- TODO this could also produce NullableOrNot
instance (
    ReifyPrimCodec mods Encoders.Value a
  ) => ReifyPrimCodec (Nullable grd : mods) Encoder (Maybe a) where
    reifyPrimCodec =
      Encoder (columnEncoderNullable encoder) (ignoreEncoder encoder)
      where
        encoder = reifyPrimCodec @mods
