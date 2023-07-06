module Sqel.Error.PrimCodec where

import Type.Errors (ErrorMessage)

import Sqel.Data.Mods.Sort (ModSort (CreateFinal, ToFinal))
import Sqel.Kind.Error (BulletedLines, Quoted)

type ModTycon :: Type -> ErrorMessage
type family ModTycon mod where
  ModTycon (mod _ _ _) = ToErrorMessage mod
  ModTycon (mod _ _) = ToErrorMessage mod
  ModTycon (mod _) = ToErrorMessage mod
  ModTycon mod = ToErrorMessage mod

type ModWc :: Type -> ErrorMessage
type family ModWc mod where
  ModWc (mod _ _ _) = mod <> " _ _ _"
  ModWc (mod _ _) = mod <> " _ _"
  ModWc (mod _) = mod <> " _"
  ModWc mod = ToErrorMessage mod

type ModArg :: Type -> ErrorMessage
type family ModArg mod where
  ModArg (mod _ _ _) = mod <> " x y z"
  ModArg (mod _ _) = mod <> " x y"
  ModArg (mod _) = mod <> " x"
  ModArg mod = ToErrorMessage mod

type Sort c s t = "'" <> c <> s <> " for " <> Quoted t

type ModUnsupported :: Symbol -> Symbol -> Type -> ErrorMessage
type family ModUnsupported sort c mod where
  ModUnsupported sort c mod =
    "The mod " <> Quoted (ModTycon mod) <> " is not supported for " <> sort <> "." %
    "If it is irrelevant for " <> sort <> ", define:" %
    "  type instance " <> c <> "Mod (" <> ModWc mod <> ") = '" <> c <> "Skip" %
    "If it should create or modify a " <> sort <> ", use another constructor on the rhs:" %
    BulletedLines [
      Sort c "Transform" "Value -> Value",
      Sort c "Create" "Value",
      Sort c "CreateRow" "Row",
      Sort c "ToRow" "Value -> Row"
    ]

-- TODO a/an
type family FinalBeforeValue codec final mod :: k where
  FinalBeforeValue codec final mod =
    TypeError (
      "While handling a column mod for a " <> codec <> ":" %
      Quoted (ModTycon mod) %
      "It is declared as creating a " <> Quoted final <> ", but there is a " <> Quoted "Value" <>
      " mod applied after it." %
      "Try changing the order of mods for this column." %
      "Note that " <> Quoted "Prim" <> " might automatically add mods like " <> Quoted "Nullable" %
      "before your explicit mod, in which case you'll have to make all mods explicit."
    )

type ModOrderError :: Symbol -> Symbol -> Type -> k0 -> k
type family ModOrderError codec final mod sort where
  ModOrderError codec final mod 'ToFinal =
    FinalBeforeValue codec final mod
  ModOrderError codec final mod 'CreateFinal =
    FinalBeforeValue codec final mod

type ArrayMismatch :: (Type -> Type) -> Type -> k
type family ArrayMismatch f a where
  ArrayMismatch f a =
    TypeError (
      "A column of type " <> Quoted a <> " was declared as an array with functor " <> Quoted f <> "," %
      "but the functor doesn't match the type."
    )

type MissingCodecInstance :: Symbol -> Type -> k0 -> k1 -> k2 -> k3 -> k
type family MissingCodecInstance codec mod sort cls head rest where
  MissingCodecInstance codec mod sort cls head rest =
    TypeError (
      "While handling a column mod for a " <> codec <> ":" %
      Quoted (ModTycon mod) %
      "It is declared as " <> sort %
      "The implementation must be provided as an instance:" %
      "  instance " <> cls <> " error (" <> ModArg mod <> ") " <> head %
      rest
    )
