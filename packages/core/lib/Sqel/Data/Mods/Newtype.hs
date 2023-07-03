module Sqel.Data.Mods.Newtype where

import Sqel.Class.ReifyDecoder (DecoderMod, TransformDecoder (transformDecoder))
import Sqel.Class.ReifyEncoder (EncoderMod, TransformEncoder (transformEncoder))
import Sqel.Data.Mods.Sort (ModSort (Transform))
import Sqel.SOP.Newtype (UnwrapNewtype (unwrapNewtype, wrapNewtype))

type Newtype :: Type -> Type -> Type
data Newtype a w

type instance DecoderMod (Newtype _ _) = 'Transform
type instance EncoderMod (Newtype _ _) = 'Transform

instance UnwrapNewtype a w => TransformEncoder error (Newtype a w) a w where
  transformEncoder = contramap unwrapNewtype

instance UnwrapNewtype a w => TransformDecoder error (Newtype a w) a w where
  transformDecoder = fmap wrapNewtype
