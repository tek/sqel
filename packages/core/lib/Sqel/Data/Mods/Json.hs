module Sqel.Data.Mods.Json where

import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON, ToJSON)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders

import Sqel.Class.ReifyDecoder (ReifyDecoder (reifyDecoder))
import Sqel.Class.ReifyEncoder (ReifyEncoder (reifyEncoder))

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

instance ToJSON a => ReifyEncoder (Json : mods) a where
  reifyEncoder = jsonEncoder

instance FromJSON a => ReifyDecoder (Json : mods) a where
  reifyDecoder = jsonDecoder

instance ToJSON a => ReifyEncoder (Jsonb : mods) a where
  reifyEncoder = jsonEncoder

instance FromJSON a => ReifyDecoder (Jsonb : mods) a where
  reifyDecoder = jsonDecoder
