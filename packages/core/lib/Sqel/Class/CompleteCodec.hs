module Sqel.Class.CompleteCodec where

import qualified Hasql.Decoders as Decoders
import Hasql.Decoders (Row)
import qualified Hasql.Encoders as Encoders
import Hasql.Encoders (Params)

import Sqel.Codec (ColumnDecoder (columnDecoder), ColumnEncoder (columnEncoder, columnEncoderIgnore), ignoreDecoder)
import Sqel.Data.Codec (Decoder (Decoder), Encoder (Encoder))

type CompleteCodec :: (Type -> Type) -> (Type -> Type) -> Constraint
class CompleteCodec b b' where
  completeCodec :: b' a -> b a

instance CompleteCodec Decoder Decoders.Value where
  completeCodec decoder = Decoder (columnDecoder decoder) (void ignoreDecoder)

instance CompleteCodec Encoder Encoders.Value where
  completeCodec encoder = Encoder (columnEncoder encoder) (columnEncoderIgnore encoder)

instance CompleteCodec Row Decoders.Value where
  completeCodec decoder = columnDecoder decoder

instance CompleteCodec Params Encoders.Value where
  completeCodec encoder = columnEncoder encoder
