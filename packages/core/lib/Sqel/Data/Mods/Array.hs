module Sqel.Data.Mods.Array where

import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders

import Sqel.Class.ReifyDecoder (ReifyDecoder (reifyDecoder))
import Sqel.Class.ReifyEncoder (ReifyEncoder (reifyEncoder))
import qualified Sqel.Codec.PrimDecoder as PrimDecoder
import qualified Sqel.Codec.PrimEncoder as PrimEncoder
import Sqel.Error.PrimCodec (ArrayMismatch)

type Array :: (Type -> Type) -> Type
data Array f

type CheckError :: (Type -> Type) -> Type -> k
type family CheckError f a where
  CheckError f (f _) = ()
  CheckError f a = ArrayMismatch f a

------------------------------------------------------------------------------------------------------------------------

type ArrayEncoder :: Void -> [Type] -> (Type -> Type) -> Type -> Constraint
class ArrayEncoder error mods f a where
  arrayEncoder :: Encoders.Value a

instance (
    Foldable f,
    ReifyEncoder mods a
  ) => ArrayEncoder error mods f (f a) where
    arrayEncoder = PrimEncoder.arrayEncoder (reifyEncoder @mods)

------------------------------------------------------------------------------------------------------------------------

type ArrayDecoder :: Void -> [Type] -> (Type -> Type) -> Type -> Constraint
class ArrayDecoder error mods f a where
  arrayDecoder :: Decoders.Value a

instance (
    ReifyDecoder mods a,
    PrimDecoder.ArrayDecoder f a
  ) => ArrayDecoder error mods f (f a) where
    arrayDecoder = PrimDecoder.arrayDecoder (reifyDecoder @mods)

------------------------------------------------------------------------------------------------------------------------

instance (
    error ~ CheckError f a,
    ArrayEncoder error mods f a
  ) => ReifyEncoder (Array f : mods) a where
    reifyEncoder = arrayEncoder @error @mods @f

instance (
    error ~ CheckError f a,
    ArrayDecoder error mods f a
  ) => ReifyDecoder (Array f : mods) a where
    reifyDecoder = arrayDecoder @error @mods @f
