module Sqel.Codec.PrimDecoder where

import qualified Chronos as Chronos
import qualified Data.Map.Strict as Map
import Data.Scientific (Scientific)
import qualified Data.Set as Set
import Data.Time (Day, DiffTime, LocalTime (LocalTime), TimeOfDay (TimeOfDay), TimeZone, UTCTime, toModifiedJulianDay)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Hasql.Decoders (
  Value,
  bool,
  bytea,
  char,
  custom,
  date,
  enum,
  float4,
  float8,
  int2,
  int4,
  int8,
  interval,
  listArray,
  nonNullable,
  numeric,
  refine,
  text,
  time,
  timestamp,
  timestamptz,
  timetz,
  uuid,
  vectorArray,
  )
import Path (Abs, Dir, File, Path, Rel, parseAbsDir, parseAbsFile, parseRelDir, parseRelFile)
import Prelude hiding (Enum, bool)

import Sqel.SOP.Enum (EnumTable (enumTable))

class PrimDecoder a where
  primDecoder :: Value a

instance PrimDecoder () where
  primDecoder =
    void bool

instance PrimDecoder Bool where
  primDecoder =
    bool

instance PrimDecoder Int16 where
  primDecoder =
    int2

instance PrimDecoder Int32 where
  primDecoder =
    int4

instance PrimDecoder Int64 where
  primDecoder =
    int8

instance PrimDecoder Int where
  primDecoder =
    fromIntegral <$> int8

instance PrimDecoder Float where
  primDecoder =
    float4

instance PrimDecoder Double where
  primDecoder =
    float8

instance PrimDecoder Scientific where
  primDecoder =
    numeric

instance PrimDecoder Char where
  primDecoder =
    char

instance PrimDecoder Text where
  primDecoder =
    text

instance PrimDecoder ByteString where
  primDecoder =
    bytea

instance PrimDecoder Day where
  primDecoder =
    date

instance PrimDecoder LocalTime where
  primDecoder =
    timestamp

instance PrimDecoder UTCTime where
  primDecoder =
    timestamptz

instance PrimDecoder TimeOfDay where
  primDecoder =
    time

instance PrimDecoder (TimeOfDay, TimeZone) where
  primDecoder =
    timetz

instance PrimDecoder DiffTime where
  primDecoder =
    interval

instance PrimDecoder UUID where
  primDecoder =
    uuid

decodePath ::
  Show e =>
  (String -> Either e (Path b t)) ->
  Bool ->
  ByteString ->
  Either Text (Path b t)
decodePath parse _ =
  first show . parse . decodeUtf8

instance PrimDecoder (Path Abs File) where
  primDecoder =
    custom (decodePath parseAbsFile)

instance PrimDecoder (Path Abs Dir) where
  primDecoder =
    custom (decodePath parseAbsDir)

instance PrimDecoder (Path Rel File) where
  primDecoder =
    custom (decodePath parseRelFile)

instance PrimDecoder (Path Rel Dir) where
  primDecoder =
    custom (decodePath parseRelDir)

dayToChronos :: Day -> Chronos.Date
dayToChronos =
  Chronos.dayToDate . Chronos.Day . fromIntegral . toModifiedJulianDay

instance PrimDecoder Chronos.Date where
  primDecoder =
    dayToChronos <$> date

instance PrimDecoder Chronos.Time where
  primDecoder =
    Chronos.Time <$> int8

chronosToTimeOfDay :: TimeOfDay -> Chronos.TimeOfDay
chronosToTimeOfDay (TimeOfDay h m ns) =
  Chronos.TimeOfDay h m (round (ns * 1000000000))

localTimeToDatetime :: LocalTime -> Chronos.Datetime
localTimeToDatetime (LocalTime d t) =
  Chronos.Datetime (dayToChronos d) (chronosToTimeOfDay t)

instance PrimDecoder Chronos.Datetime where
  primDecoder =
    localTimeToDatetime <$> primDecoder

class ArrayDecoder f a where
  arrayDecoder :: Value a -> Value (f a)

instance ArrayDecoder [] a where
  arrayDecoder =
    listArray . nonNullable

instance ArrayDecoder NonEmpty a where
  arrayDecoder =
    refine (maybeToRight "no elements in NonEmpty field" . nonEmpty) .
    listArray .
    nonNullable

instance ArrayDecoder Vector a where
  arrayDecoder =
    vectorArray . nonNullable

instance (
    Ord a
  ) => ArrayDecoder Set a where
  arrayDecoder =
    refine (Right . Set.fromList) .
    listArray .
    nonNullable

enumDecoder ::
  EnumTable a =>
  Value a
enumDecoder =
  enum (`Map.lookup` enumTable)

readDecoder ::
  Read a =>
  Value a
readDecoder =
  enum (readMaybe . fromText)
