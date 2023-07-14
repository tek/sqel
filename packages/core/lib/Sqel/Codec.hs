module Sqel.Codec where

import qualified Chronos as Chronos
import Data.Time (Day, DiffTime, LocalTime, TimeOfDay, TimeZone, UTCTime)
import Data.UUID (UUID)
import qualified Hasql.Decoders as Hasql
import qualified Hasql.Decoders as Decoders
import Hasql.Decoders (Row, custom)
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Encoders as Encoder
import Hasql.Encoders (Params)
import Path (Path)

import qualified Sqel.Codec.PrimDecoder as PrimDecoder
import Sqel.Codec.PrimDecoder (PrimDecoder)
import qualified Sqel.Codec.PrimEncoder as PrimEncoder
import Sqel.Codec.PrimEncoder (PrimEncoder)
import Sqel.Data.PgType (PgPrimName, pgPrimName)
import Sqel.Kind.Error (Quoted)

ignoreEncoder :: Encoder.Value a -> Params b
ignoreEncoder v =
  const Nothing >$< Encoders.param (Encoders.nullable v)

ignoreDecoder :: Row (Maybe a)
ignoreDecoder =
  join <$> Hasql.column (Hasql.nullable (custom \ _ _ -> pure Nothing))

class ColumnEncoder f where
  columnEncoder :: f a -> Params a
  columnEncoderNullable :: f a -> Params (Maybe a)
  columnEncoderIgnore :: f a -> Params b

instance ColumnEncoder Encoders.Value where
  columnEncoder =
    Encoders.param . Encoders.nonNullable
  columnEncoderNullable =
    Encoders.param . Encoders.nullable
  columnEncoderIgnore =
    ignoreEncoder

class ColumnDecoder f where
  columnDecoder :: f a -> Row a
  columnDecoderNullable :: f a -> Row (Maybe a)

instance ColumnDecoder Decoders.Value where
  columnDecoder =
    Decoders.column . Decoders.nonNullable
  columnDecoderNullable =
    Decoders.column . Decoders.nullable

-- TODO proper error handling in arg, re-enable warning
type PrimColumn :: Void -> Type -> Constraint
class PrimColumn error a where
  primDecoder :: Decoders.Value a
  default primDecoder :: PrimDecoder a => Decoders.Value a
  primDecoder = PrimDecoder.primDecoder

  primEncoder :: Encoders.Value a
  default primEncoder :: PrimEncoder a => Encoders.Value a
  primEncoder = PrimEncoder.primEncoder

  type PrimTypeName a :: Symbol

  pgType :: PgPrimName
  default pgType :: KnownSymbol (PrimTypeName a) => PgPrimName
  pgType = pgPrimName @(PrimTypeName a)

instance PrimColumn error Bool where type PrimTypeName Bool = "boolean"
instance PrimColumn error Int where type PrimTypeName Int = "bigint"
instance PrimColumn error Int64 where type PrimTypeName Int64 = "bigint"
instance PrimColumn error Double where type PrimTypeName Double = "double precision"
instance PrimColumn error Text where type PrimTypeName Text = "text"
instance PrimColumn error ByteString where type PrimTypeName ByteString = "bytes"
instance PrimColumn error UUID where type PrimTypeName UUID = "uuid"
instance PrimColumn error Day where type PrimTypeName Day = "date"
instance PrimColumn error LocalTime where type PrimTypeName LocalTime = "timestamp without time zone"
instance PrimColumn error UTCTime where type PrimTypeName UTCTime = "timestamp with time zone"
instance PrimColumn error TimeOfDay where type PrimTypeName TimeOfDay = "time without time zone"
instance PrimColumn error (TimeOfDay, TimeZone) where type PrimTypeName (TimeOfDay, TimeZone) = "time with time zone"
instance PrimColumn error DiffTime where type PrimTypeName DiffTime = "interval"
instance PrimColumn error Chronos.Date where type PrimTypeName Chronos.Date = "date"
instance PrimColumn error Chronos.Time where type PrimTypeName Chronos.Time = "bigint"
instance PrimColumn error Chronos.Datetime where type PrimTypeName Chronos.Datetime = "timestamp without time zone"
instance PrimDecoder (Path b t) => PrimColumn error (Path b t) where type PrimTypeName (Path b t) = "text"
instance PrimColumn error () where type PrimTypeName () = "boolean"

-- TODO improve and adapt to new dsl
-- special case for Maybe and []
type NoPrimColumn :: Type -> k
type family NoPrimColumn a where
  NoPrimColumn a =
    TypeError (
      "A column of type " <> Quoted a <> " was declared as primitive," %
      "but there is no instance of " <> Quoted "PrimColumn" <> " for that type." %
      "If it is a newtype, ensure that it has " <> Quoted "Generic" <> " and use " <> Quoted "Newtype" <> "." %
      "If it is a sequence type, use " <> Quoted "Array" <> "." %
      "If it is a " <> Quoted Maybe <> " column, use " <> Quoted "Nullable" <> "."
    )

type PrimColumnOrError :: Type -> Constraint
class PrimColumnOrError a where
  primDecoderOrError :: Decoders.Value a
  primEncoderOrError :: Encoders.Value a
  type PrimTypeNameOrError a :: Symbol
  pgTypeOrError :: PgPrimName

instance (
    error ~ NoPrimColumn a,
    PrimColumn error a
  ) => PrimColumnOrError a where
    primDecoderOrError = primDecoder @error
    primEncoderOrError = primEncoder @error
    type PrimTypeNameOrError a = PrimTypeName a
    pgTypeOrError = pgType @error @a
