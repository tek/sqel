module Sqel.Data.Mods.Array where

import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders

import Sqel.Class.ReifyDecoder (DecoderMod, TransformDecoder (transformDecoder))
import Sqel.Class.ReifyEncoder (EncoderMod, TransformEncoder (transformEncoder))
import Sqel.Codec.PrimDecoder (ArrayDecoder (arrayDecoder))
import Sqel.Codec.PrimEncoder (arrayEncoder)
import Sqel.Data.Mods.Sort (ModSort (Transform))
import Sqel.Error.PrimCodec (ArrayMismatch)

type Array :: (Type -> Type) -> Type
data Array f

type ShapeError :: (Type -> Type) -> Type -> k
type family ShapeError f a where
  ShapeError f (f _) = ()
  ShapeError f a = ArrayMismatch f a

type instance DecoderMod (Array _) = 'Transform
type instance EncoderMod (Array _) = 'Transform

type CheckedEncoder :: Void -> (Type -> Type) -> Type -> Type -> Constraint
class CheckedEncoder error f a b | a -> b where
  checkedEncoder :: Encoders.Value b -> Encoders.Value a

instance Foldable f => CheckedEncoder error f (f a) a where
  checkedEncoder = arrayEncoder

instance (
    shapeError ~ ShapeError f a,
    CheckedEncoder shapeError f a b
  ) => TransformEncoder error (Array f) a b where
    transformEncoder = checkedEncoder @shapeError @f

type CheckedDecoder :: Void -> (Type -> Type) -> Type -> Type -> Constraint
class CheckedDecoder error f a b | a -> b where
  checkedDecoder :: Decoders.Value b -> Decoders.Value a

instance ArrayDecoder f a => CheckedDecoder error f (f a) a where
  checkedDecoder = arrayDecoder

instance (
    shapeError ~ ShapeError f a,
    CheckedDecoder shapeError f a b
  ) => TransformDecoder error (Array f) a b where
    transformDecoder = checkedDecoder @shapeError @f
