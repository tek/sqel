module Sqel.Data.Mods.Nullable where

import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders

import Sqel.Class.ReifyDecoder (DecoderMod, ToRow (toRow))
import Sqel.Class.ReifyEncoder (EncoderMod, ToParams (toParams))
import Sqel.Data.Mods.Sort (ModSort (Skip, ToFinal))

data NullableConf = NullableConf Bool (Maybe (Type -> Type))

-- | The flag controls whether the column should be considered a match in a query when it is null, irrespective
-- of the query value.
-- This only has an effect when used with a query type.
type Nullable :: NullableConf -> Type
data Nullable conf

type NullableMaybe = Nullable ('NullableConf 'False ('Just Maybe))
type NullableCon = Nullable ('NullableConf 'False 'Nothing)

type instance DecoderMod (Nullable ('NullableConf _ ('Just _))) = 'ToFinal
type instance DecoderMod (Nullable ('NullableConf _ 'Nothing)) = 'Skip

-- TODO can we support types other than Maybe
instance ToRow error (Nullable ('NullableConf grd ('Just Maybe))) (Maybe a) a where
  toRow = Decoders.column . Decoders.nullable

type instance EncoderMod (Nullable ('NullableConf _ ('Just _))) = 'ToFinal
type instance EncoderMod (Nullable ('NullableConf _ 'Nothing)) = 'Skip

instance ToParams error (Nullable ('NullableConf grd ('Just Maybe))) (Maybe a) a where
  toParams = Encoders.param . Encoders.nullable
