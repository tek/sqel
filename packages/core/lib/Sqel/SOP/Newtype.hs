module Sqel.SOP.Newtype where

import Generics.SOP.GGP (GCode, GDatatypeInfoOf)
import Generics.SOP.Type.Metadata (DatatypeInfo (Newtype))
import Type.Errors (DelayError, ErrorMessage)
import Unsafe.Coerce (unsafeCoerce)

import Sqel.SOP.Error (Quoted, QuotedType)
import Sqel.SOP.HasGeneric (HasGeneric)

type ErrHead :: Type -> ErrorMessage
type ErrHead a =
  "A field of type " <> Quoted a <> " was declared as a newtype column,"

type NoGenericError :: Type -> ErrorMessage
type NoGenericError a =
  "but " <> Quoted a <> " does not have an instance of " <> Quoted "Generic" <> "." %
  "You can add it like this:" %
  Quoted "newtype MyType = MyType Text deriving Generic"

-- TODO try re-adding Coercible column
-- "If you want to use " <> Quoted "Coercible" <> " instead, use " <> Quoted "primCoerce" <> "."

type NotNewtypeError a =
  "but " <> QuotedType a <> " is not a newtype."

type UN_CheckNewtype :: Void -> [[Type]] -> DatatypeInfo -> Type -> Type -> Constraint
class UN_CheckNewtype err ass info a w | ass info a -> w where
  un_checkNewtype_unwrap :: a -> w
  un_checkNewtype_wrap :: w -> a

instance UN_CheckNewtype err '[ '[w] ] ('Newtype mn dn ci) a w where
  un_checkNewtype_unwrap = unsafeCoerce
  un_checkNewtype_wrap = unsafeCoerce

type UN_CheckGeneric :: Void -> [[Type]] -> DatatypeInfo -> Type -> Type -> Constraint
class UN_CheckGeneric err ass info a w | ass info a -> w where
  un_checkGeneric_unwrap :: a -> w
  un_checkGeneric_wrap :: w -> a

instance (
    err ~ DelayError (ErrHead a % NotNewtypeError a),
    UN_CheckNewtype err '[] info a w
  ) => UN_CheckGeneric genErr '[] info a w where
    un_checkGeneric_unwrap = un_checkNewtype_unwrap @err @'[] @info
    un_checkGeneric_wrap = un_checkNewtype_wrap @err @'[] @info

instance (
    err ~ DelayError (ErrHead a % NotNewtypeError a),
    UN_CheckNewtype err (x : xs) info a w
  ) => UN_CheckGeneric genErr (x : xs) info a w where
    un_checkGeneric_unwrap = un_checkNewtype_unwrap @err @(x : xs) @info
    un_checkGeneric_wrap = un_checkNewtype_wrap @err @(x : xs) @info

type UnwrapNewtype :: Type -> Type -> Constraint
class UnwrapNewtype a w | a -> w where
  unwrapNewtype :: a -> w
  wrapNewtype :: w -> a

instance (
  HasGeneric a flag,
  err ~ DelayError (ErrHead a % NoGenericError a),
  UN_CheckGeneric err (GCode a) (GDatatypeInfoOf a) a w
  ) => UnwrapNewtype a w where
    unwrapNewtype = un_checkGeneric_unwrap @err @(GCode a) @(GDatatypeInfoOf a)
    wrapNewtype = un_checkGeneric_wrap @err @(GCode a) @(GDatatypeInfoOf a)
