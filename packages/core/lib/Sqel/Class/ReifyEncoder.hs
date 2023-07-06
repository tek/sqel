module Sqel.Class.ReifyEncoder where

import Fcf (Pure)
import qualified Hasql.Encoders as Encoders
import Hasql.Encoders (Params, Value)
import Type.Errors (DelayError, IfStuck)

import Sqel.Class.CompleteCodec (CompleteCodec (completeCodec))
import Sqel.Codec (PrimColumnOrError (primEncoderOrError))
import Sqel.Data.Mods.Sort (ModSort (Create, CreateFinal, Skip, ToFinal, Transform))
import Sqel.Error.PrimCodec (MissingCodecInstance, ModOrderError, ModUnsupported)
import Sqel.Kind.Error (Quoted)

type EncoderMod :: Type -> ModSort
type family EncoderMod mod

type EncoderModE :: Type -> ModSort
type family EncoderModE mod where
  EncoderModE mod =
    IfStuck (EncoderMod mod) (DelayError (ModUnsupported "encoders" "Encoder" mod)) (Pure (EncoderMod mod))

------------------------------------------------------------------------------------------------------------------------

class CreateEncoder error mod a where
  createEncoder :: Value a

type MissingCreateEncoder :: Type -> k
type family MissingCreateEncoder mod where
  MissingCreateEncoder mod =
    MissingCodecInstance "encoder" mod ("directly creating a " <>  Quoted "Value" <> ".") "CreateEncoder" "a" ""

------------------------------------------------------------------------------------------------------------------------

type TransformEncoder :: Void -> Type -> Type -> Type -> Constraint
class TransformEncoder error mod a b | mod a -> b where
  transformEncoder :: Value b -> Value a

type MissingTransformEncoder :: Type -> k
type family MissingTransformEncoder mod where
  MissingTransformEncoder mod =
    MissingCodecInstance "encoder" mod ("transforming a " <> Quoted "Value" <> ".") "TransformEncoder" "a b"
    (
      "where " <> Quoted "a" <> " is the type of the new " <> Quoted "Value" <> " and " <> Quoted "b" %
      "is the type of the " <> Quoted "Value" <> " that this transformation expects."
    )

------------------------------------------------------------------------------------------------------------------------

type TriageEncoderValueMod :: Void -> ModSort -> Type -> [Type] -> Type -> Constraint
class TriageEncoderValueMod error sort mod mods a where
  triageEncoderValueMod :: Value a

instance ReifyEncoderValue mods a => TriageEncoderValueMod error 'Skip mod mods a where
  triageEncoderValueMod = reifyEncoderValue @mods

instance (
    ReifyEncoderValue mods b,
    error ~ MissingTransformEncoder mod,
    TransformEncoder error mod a b
  ) => TriageEncoderValueMod orderError 'Transform mod mods a where
    triageEncoderValueMod = transformEncoder @error @mod (reifyEncoderValue @mods)

instance (
    error ~ MissingCreateEncoder mod,
    CreateEncoder error mod a
  ) => TriageEncoderValueMod orderError 'Create mod mods a where
  triageEncoderValueMod = createEncoder @error @mod

------------------------------------------------------------------------------------------------------------------------

type ReifyEncoderValue :: [Type] -> Type -> Constraint
class ReifyEncoderValue mods a where
  reifyEncoderValue :: Encoders.Value a

instance (
    sort ~ EncoderModE mod,
    error ~ ModOrderError "encoder" "Params" mod sort,
    TriageEncoderValueMod error sort mod mods a
  ) => ReifyEncoderValue (mod : mods) a where
    reifyEncoderValue = triageEncoderValueMod @error @sort @mod @mods

instance PrimColumnOrError a => ReifyEncoderValue '[] a where
  reifyEncoderValue = primEncoderOrError

------------------------------------------------------------------------------------------------------------------------

type CreateParams :: Void -> Type -> Type -> Constraint
class CreateParams error mod a where
  createParams :: Params a

type MissingCreateParams :: Type -> k
type family MissingCreateParams mod where
  MissingCreateParams mod =
    MissingCodecInstance "encoder" mod ("directly creating a " <>  Quoted "Params" <> ".") "CreateParams" "a" ""

type ToParams :: Void -> Type -> Type -> Type -> Constraint
class ToParams error mod a b | mod a -> b where
  toParams :: Value b -> Params a

type MissingToParams :: Type -> k
type family MissingToParams mod where
  MissingToParams mod =
    MissingCodecInstance "encoder" mod ("transforming a " <> Quoted "Value" <> " to a " <> Quoted "Params" <> ".") "ToParams"
    "a b"
    (
      "where " <> Quoted "a" <> " is the final row type and " <> Quoted "b" <> " is the type of the " <>
      Quoted "Value" <> " that this transformation expects."
    )

------------------------------------------------------------------------------------------------------------------------

type TriageEncoderMod :: ModSort -> Type -> [Type] -> Type -> Constraint
class TriageEncoderMod sort mod mods a where
  triageEncoderMod :: Params a

instance ReifyEncoder mods a => TriageEncoderMod 'Skip mod mods a where
  triageEncoderMod = reifyEncoder @mods

instance (
    error ~ MissingCreateParams mod,
    CreateParams error mod a
  ) => TriageEncoderMod 'CreateFinal mod mods a where
    triageEncoderMod = createParams @error @mod

instance (
    ReifyEncoderValue mods b,
    error ~ MissingToParams mod,
    ToParams error mod a b
  ) => TriageEncoderMod 'ToFinal mod mods a where
    triageEncoderMod = toParams @error @mod (reifyEncoderValue @mods)

instance ReifyEncoderValue (mod : mods) a => TriageEncoderMod 'Create mod mods a where
  triageEncoderMod = completeCodec (reifyEncoderValue @(mod : mods))

instance ReifyEncoderValue (mod : mods) a => TriageEncoderMod 'Transform mod mods a where
  triageEncoderMod = completeCodec (reifyEncoderValue @(mod : mods))

------------------------------------------------------------------------------------------------------------------------

type ReifyEncoder :: [Type] -> Type -> Constraint
class ReifyEncoder mods a where
  reifyEncoder :: Params a

instance (
    sort ~ EncoderModE mod,
    TriageEncoderMod sort mod mods a
  ) => ReifyEncoder (mod : mods) a where
    reifyEncoder = triageEncoderMod @sort @mod @mods

instance PrimColumnOrError a => ReifyEncoder '[] a where
  reifyEncoder = completeCodec primEncoderOrError
