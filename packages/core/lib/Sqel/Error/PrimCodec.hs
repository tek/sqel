module Sqel.Error.PrimCodec where

import Type.Errors (ErrorMessage)

import Sqel.SOP.Error (Quoted)

type ModUnsupported :: Symbol -> Symbol -> Type -> ErrorMessage
type ModUnsupported desc c mod =
      "The mod " <> Quoted mod <> " is unsupported for " <> desc <> "s." %
      "If you want to ignore the mod, define:" %
      "instance ReifyPrim" <> c <> " mods a =>" %
      "  ReifyPrim" <> c <> " (" <> mod <> " : mods) a where" %
      "    reifyPrim" <> c <> " = reifyPrim" <> c <> " @mods"

type ArrayMismatch :: (Type -> Type) -> Type -> k
type family ArrayMismatch f a where
  ArrayMismatch f a =
    TypeError (
      "A column of type " <> Quoted a <> " was declared as an array with functor " <> Quoted f <> "," %
      "but the functor doesn't match the type."
    )
