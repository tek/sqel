module Sqel.Class.ReifyDecoder where

import Fcf (Pure)
import qualified Hasql.Decoders as Decoders
import Hasql.Decoders (Row, Value)
import Type.Errors (DelayError, IfStuck)

import Sqel.Class.CompleteCodec (CompleteCodec (completeCodec))
import Sqel.Codec (PrimColumnOrError (primDecoderOrError))
import Sqel.Data.Mods.Sort (ModSort (Create, CreateFinal, Skip, ToFinal, Transform))
import Sqel.Error.PrimCodec (MissingCodecInstance, ModOrderError, ModUnsupported)
import Sqel.Kind.Error (Quoted)

type DecoderMod :: Type -> ModSort
type family DecoderMod mod

type DecoderModE :: Type -> ModSort
type family DecoderModE mod where
  DecoderModE mod =
    IfStuck (DecoderMod mod) (DelayError (ModUnsupported "decoders" "Decoder" mod)) (Pure (DecoderMod mod))

------------------------------------------------------------------------------------------------------------------------

type CreateDecoder :: Void -> Type -> Type -> Constraint
class CreateDecoder error mod a where
  createDecoder :: Value a

type MissingCreateDecoder :: Type -> k
type family MissingCreateDecoder mod where
  MissingCreateDecoder mod =
    MissingCodecInstance "decoder" mod ("directly creating a " <>  Quoted "Value" <> ".") "CreateDecoder" "a" ""

------------------------------------------------------------------------------------------------------------------------

type TransformDecoder :: Void -> Type -> Type -> Type -> Constraint
class TransformDecoder error mod a b | mod a -> b where
  transformDecoder :: Value b -> Value a

type MissingTransformDecoder :: Type -> k
type family MissingTransformDecoder mod where
  MissingTransformDecoder mod =
    MissingCodecInstance "decoder" mod ("transforming a " <> Quoted "Value" <> ".") "TransformDecoder" "a b"
    (
      "where " <> Quoted "a" <> " is the type of the new " <> Quoted "Value" <> " and " <> Quoted "b" %
      "is the type of the " <> Quoted "Value" <> " that this transformation expects."
    )

------------------------------------------------------------------------------------------------------------------------

type TriageDecoderValueMod :: Void -> ModSort -> Type -> [Type] -> Type -> Constraint
class TriageDecoderValueMod error sort mod mods a where
  triageDecoderValueMod :: Value a

instance ReifyDecoderValue mods a => TriageDecoderValueMod error 'Skip mod mods a where
  triageDecoderValueMod = reifyDecoderValue @mods

instance (
    ReifyDecoderValue mods b,
    error ~ MissingTransformDecoder mod,
    TransformDecoder error mod a b
  ) => TriageDecoderValueMod orderError 'Transform mod mods a where
    triageDecoderValueMod = transformDecoder @error @mod (reifyDecoderValue @mods)

instance (
    error ~ MissingCreateDecoder mod,
    CreateDecoder error mod a
  ) => TriageDecoderValueMod orderError 'Create mod mods a where
    triageDecoderValueMod = createDecoder @error @mod

------------------------------------------------------------------------------------------------------------------------

type ReifyDecoderValue :: [Type] -> Type -> Constraint
class ReifyDecoderValue mods a where
  reifyDecoderValue :: Decoders.Value a

instance (
    sort ~ DecoderModE mod,
    error ~ ModOrderError "decoder" "Row" mod sort,
    TriageDecoderValueMod error sort mod mods a
  ) => ReifyDecoderValue (mod : mods) a where
    reifyDecoderValue = triageDecoderValueMod @error @sort @mod @mods

instance PrimColumnOrError a => ReifyDecoderValue '[] a where
  reifyDecoderValue = primDecoderOrError

------------------------------------------------------------------------------------------------------------------------

type CreateRow :: Void -> Type -> Type -> Constraint
class CreateRow error mod a where
  createRow :: Row a

type MissingCreateRow :: Type -> k
type family MissingCreateRow mod where
  MissingCreateRow mod =
    MissingCodecInstance "decoder" mod ("directly creating a " <>  Quoted "Row" <> ".") "CreateRow" "a" ""

------------------------------------------------------------------------------------------------------------------------

type ToRow :: Void -> Type -> Type -> Type -> Constraint
class ToRow error mod a b | mod a -> b where
  toRow :: Value b -> Row a

type MissingToRow :: Type -> k
type family MissingToRow mod where
  MissingToRow mod =
    MissingCodecInstance "decoder" mod ("transforming a " <> Quoted "Value" <> " to a " <> Quoted "Row" <> ".") "ToRow"
    "a b"
    (
      "where " <> Quoted "a" <> " is the final row type and " <> Quoted "b" <> " is the type of the " <>
      Quoted "Value" <> " that this transformation expects."
    )

------------------------------------------------------------------------------------------------------------------------

-- TODO triage classes could be generalized
type TriageDecoderMod :: ModSort -> Type -> [Type] -> Type -> Constraint
class TriageDecoderMod sort mod mods a where
  triageDecoderMod :: Row a

instance ReifyDecoder mods a => TriageDecoderMod 'Skip mod mods a where
  triageDecoderMod = reifyDecoder @mods

instance (
    error ~ MissingCreateRow mod,
    CreateRow error mod a
  ) => TriageDecoderMod 'CreateFinal mod mods a where
    triageDecoderMod = createRow @error @mod

instance (
    ReifyDecoderValue mods b,
    error ~ MissingToRow mod,
    ToRow error mod a b
  ) => TriageDecoderMod 'ToFinal mod mods a where
    triageDecoderMod = toRow @error @mod (reifyDecoderValue @mods)

instance ReifyDecoderValue (mod : mods) a => TriageDecoderMod 'Create mod mods a where
  triageDecoderMod = completeCodec (reifyDecoderValue @(mod : mods))

instance ReifyDecoderValue (mod : mods) a => TriageDecoderMod 'Transform mod mods a where
  triageDecoderMod = completeCodec (reifyDecoderValue @(mod : mods))

------------------------------------------------------------------------------------------------------------------------

type ReifyDecoder :: [Type] -> Type -> Constraint
class ReifyDecoder mods a where
  reifyDecoder :: Row a

instance (
    sort ~ DecoderModE mod,
    TriageDecoderMod sort mod mods a
  ) => ReifyDecoder (mod : mods) a where
    reifyDecoder = triageDecoderMod @sort @mod @mods

instance PrimColumnOrError a => ReifyDecoder '[] a where
  reifyDecoder = completeCodec primDecoderOrError
