module Sqel.Data.Mods.Nullable where

import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders

import Sqel.Class.ReifyDecoder (DecoderMod, ToRow (toRow))
import Sqel.Class.ReifyEncoder (EncoderMod, ToParams (toParams))
import Sqel.Data.Mods.Sort (ModSort (ToFinal))

-- | The flag controls whether the column should be considered a match in a query when it is null, irrespective
-- of the query value.
-- This only has an effect when used with a query type.
type Nullable :: Bool -> Type
data Nullable guardQuery

type instance DecoderMod (Nullable _) = 'ToFinal

instance ToRow error (Nullable grd) (Maybe a) a where
  toRow = Decoders.column . Decoders.nullable

type instance EncoderMod (Nullable _) = 'ToFinal

instance ToParams error (Nullable grd) (Maybe a) a where
  toParams = Encoders.param . Encoders.nullable
