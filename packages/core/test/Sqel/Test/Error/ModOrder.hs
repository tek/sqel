{-# options_ghc -fdefer-type-errors -Wno-deferred-type-errors #-}

module Sqel.Test.Error.ModOrder where

import Prelude hiding (Mod)

import Sqel.Class.ReifyDecoder (DecoderMod, TransformDecoder (transformDecoder))
import Sqel.Class.ReifyEncoder (EncoderMod, TransformEncoder (transformEncoder))
import Sqel.Class.ReifySqel (sqel)
import Sqel.Data.Codec (Codec (Codec), Encoder (Encoder))
import Sqel.Data.Mods.Sort (ModSort (Transform))
import Sqel.Data.Sqel (sqelCodec)
import Sqel.Default (Sqel)
import Sqel.Dsl (Mod, Nullable, Prim, Prod, Table)

data IdMod

type instance DecoderMod IdMod = 'Transform
type instance EncoderMod IdMod = 'Transform

instance TransformEncoder error IdMod a a where
  transformEncoder = id

instance TransformDecoder error IdMod a a where
  transformDecoder = id

data Dat =
  Dat {
    f1 :: Maybe Int64
  }
  deriving stock (Eq, Show, Generic)

type Table_Dat =
  Table "dat" Dat (Prod '[Mod IdMod (Nullable Prim)])

table_Dat :: Sqel Table_Dat
table_Dat =
  sqel @Table_Dat

-- | Unfortunately, we can't evaluate the Params to trigger the @TypeError@.
modOrder :: ()
modOrder = case sqelCodec table_Dat of
  Codec (Encoder !_ _) _ -> ()
