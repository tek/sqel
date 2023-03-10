module Sqel.Codec.PrimEncoder where

import qualified Chronos as Chronos
import Data.Scientific (Scientific)
import Data.Time (Day (ModifiedJulianDay), DiffTime, LocalTime (LocalTime), TimeOfDay (TimeOfDay), TimeZone, UTCTime)
import Data.UUID (UUID)
import Hasql.Encoders (
  Value,
  array,
  bool,
  bytea,
  char,
  date,
  dimension,
  element,
  float4,
  float8,
  int2,
  int4,
  int8,
  interval,
  nonNullable,
  numeric,
  text,
  time,
  timestamp,
  timestamptz,
  timetz,
  uuid,
  )
import Path (Path, toFilePath)
import Prelude hiding (bool)

class PrimEncoder d where
  primEncoder :: Value d

instance PrimEncoder () where
  primEncoder =
    True >$ bool

instance PrimEncoder Bool where
  primEncoder =
    bool

instance PrimEncoder Int16 where
  primEncoder =
    int2

instance PrimEncoder Int32 where
  primEncoder =
    int4

instance PrimEncoder Int64 where
  primEncoder =
    int8

instance PrimEncoder Int where
  primEncoder =
    contramap fromIntegral int8

instance PrimEncoder Float where
  primEncoder =
    float4

instance PrimEncoder Double where
  primEncoder =
    float8

instance PrimEncoder Scientific where
  primEncoder =
    numeric

instance PrimEncoder Char where
  primEncoder =
    char

instance PrimEncoder Text where
  primEncoder =
    text

instance PrimEncoder ByteString where
  primEncoder =
    bytea

instance PrimEncoder Day where
  primEncoder =
    date

instance PrimEncoder LocalTime where
  primEncoder =
    timestamp

instance PrimEncoder UTCTime where
  primEncoder =
    timestamptz

instance PrimEncoder TimeOfDay where
  primEncoder =
    time

instance PrimEncoder (TimeOfDay, TimeZone) where
  primEncoder =
    timetz

instance PrimEncoder DiffTime where
  primEncoder =
    interval

instance PrimEncoder UUID where
  primEncoder =
    uuid

instance PrimEncoder (Path b t) where
  primEncoder =
    (toText . toFilePath) >$< text

chronosToDay :: Chronos.Date -> Day
chronosToDay =
  ModifiedJulianDay . fromIntegral . Chronos.getDay . Chronos.dateToDay

instance PrimEncoder Chronos.Date where
  primEncoder =
    chronosToDay >$< date

instance PrimEncoder Chronos.Time where
  primEncoder =
    Chronos.getTime >$< int8

chronosToTimeOfDay :: Chronos.TimeOfDay -> TimeOfDay
chronosToTimeOfDay (Chronos.TimeOfDay h m ns) =
  TimeOfDay h m (fromMaybe 0 (realToFrac ns / 1000000000))

datetimeToLocalTime :: Chronos.Datetime -> LocalTime
datetimeToLocalTime (Chronos.Datetime d t) =
  LocalTime (chronosToDay d) (chronosToTimeOfDay t)

instance PrimEncoder Chronos.Datetime where
  primEncoder =
     datetimeToLocalTime >$< primEncoder

arrayEncoder ::
  Foldable f =>
  Value a ->
  Value (f a)
arrayEncoder =
  array .
  dimension foldl' .
  element .
  nonNullable
