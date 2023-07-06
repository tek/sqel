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
