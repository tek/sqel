module Sqel.Data.Mods.Json where

import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON, ToJSON)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders

import Sqel.Class.ReifyDecoder (CreateDecoder (createDecoder), DecoderMod)
import Sqel.Class.ReifyEncoder (CreateEncoder (createEncoder), EncoderMod)
import Sqel.Data.Mods.Sort (ModSort (Create))

data Json

data Jsonb

jsonEncoder ::
  ToJSON a =>
  Encoders.Value a
jsonEncoder =
  toStrict . Aeson.encode >$< Encoders.jsonBytes

jsonDecoder ::
  FromJSON a =>
  Decoders.Value a
jsonDecoder =
  Decoders.jsonBytes (first toText . Aeson.eitherDecodeStrict')

type instance DecoderMod Json = 'Create
type instance EncoderMod Json = 'Create

instance ToJSON a => CreateEncoder error Json a where
  createEncoder = jsonEncoder

instance FromJSON a => CreateDecoder error Json a where
  createDecoder = jsonDecoder
