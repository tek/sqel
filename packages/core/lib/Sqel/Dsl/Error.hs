module Sqel.Dsl.Error where

import Type.Errors (ErrorMessage)

import Sqel.Data.Dd (Ext0 (Ext0))
import Sqel.Data.Name (Name (Name, NameAuto))
import Sqel.Data.Sel (Sel (Sel))
import Sqel.Kind.Error (Quoted)

type ColumnNameForError :: Ext0 -> ErrorMessage
type family ColumnNameForError ext where
  ColumnNameForError ('Ext0 ('Sel ('Name name) _) _) = Quoted name
  ColumnNameForError ('Ext0 ('Sel 'NameAuto _) _) = Quoted "<unnamed>"

type TypeNamePrimError :: Symbol -> Ext0 -> Type -> k
type family TypeNamePrimError name ext a where
  TypeNamePrimError name ext a =
    TypeError (
      "Cannot set the type name of the primitive column " <> ColumnNameForError ext <> " with type " <> Quoted a <>
      " to " <> Quoted name <> "." %
      "The combinator " <> Quoted "TypeName" <> " can only be used with composite columns."
    )

type UndetGeneral :: Type -> ErrorMessage
type UndetGeneral spec =
  "If you are calling a polymorphic function that has a constraint like " <> Quoted "ReifySqel" <> "," %
  "you probably need to use a type application to specify the spec, like " <> Quoted "Prim" <> "." %
  "If the variable is supposed to be polymorphic, you need to add " <> Quoted "ReifySqel" <>
  " to its function's context" %
  "and use the variable in the type application."

type NoReify :: Type -> Type -> ErrorMessage
type family NoReify a spec where
  NoReify a spec =
    "The spec:" % Quoted spec % " given for a column of type " <> Quoted a <> " is not supported." %
    "If you intend to use it as a custom spec, you need to define:" %
    "type instance Reify a (" <> spec <> ") = <impl>" %
    "If there is an undetermined type variable in the spec:" %
    UndetGeneral spec

type NoSpec :: Type -> Type -> ErrorMessage
type family NoSpec a spec where
  NoSpec a spec =
    "The type (variable) " <> Quoted spec <> " specifying a column of type " <> Quoted a <> " is undetermined." %
    UndetGeneral spec
