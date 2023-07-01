module Sqel.Class.MatchView where

import Fcf (If)
import Type.Errors (DelayError, ErrorMessage, IfStuck, Pure)

import Sqel.Data.Dd (Dd)
import Sqel.Data.FieldPath (FieldPath (FieldPath), FieldPaths, PathEq, ShowFields)
import Sqel.Dd (DdType)
import Sqel.SOP.Error (QuotedError, QuotedType, ShowPath, Unlines)

type ColumnMessage :: Symbol -> FieldPath -> ErrorMessage
type family ColumnMessage viewType path where
  ColumnMessage viewType ('FieldPath path tpe) =
    "The " <> viewType <> " column " <> ShowPath path <> " with type " <> QuotedType tpe <>
    " does not correspond to a table column."

type CheckAvail :: [FieldPath] -> Type
type family CheckAvail fields where
  CheckAvail ('FieldPath _ t : _) = t

type AvailableColumns :: [FieldPath] -> ErrorMessage
type family AvailableColumns fields where
  AvailableColumns fields =
    "The specified table contains these fields:" %
    Unlines (ShowFields fields)

type NoViewMatch :: Symbol -> [FieldPath] -> FieldPath -> ErrorMessage
type family NoViewMatch viewType avail path where
  NoViewMatch viewType avail path =
    ColumnMessage viewType path %
    AvailableColumns avail

type NoViewMatchTable :: Dd -> [FieldPath] -> ErrorMessage
type family NoViewMatchTable table avail where
  NoViewMatchTable table avail =
    "The table for type " <> QuotedType (DdType table) <> " contains these fields:" %
    Unlines (ShowFields avail)

type UnknownMsg :: Symbol -> FieldPath -> ErrorMessage
type family UnknownMsg viewType field where
  UnknownMsg viewType ('FieldPath path tpe) =
    "This " <> viewType <> " cannot determine whether the column " <> ShowPath path <> " with type " <>
    QuotedType tpe %
    "corresponds to a table column."

type PathConstraint :: [Symbol] -> ErrorMessage
type family PathConstraint path where
  PathConstraint '[field] = "HasColumn " <> 'ShowType field
  PathConstraint path = "HasPath " <> path

type AvailStuckMsg :: Symbol -> FieldPath -> ErrorMessage
type family AvailStuckMsg viewType field where
  AvailStuckMsg viewType ('FieldPath path tpe) =
    UnknownMsg viewType ('FieldPath path tpe) %
    "This is likely due to the structure type being polymorphic or not in scope." %
    "Try adding the constraint:" %
    "  " <> QuotedError (PathConstraint path <> " " <> tpe <> " <" <> viewType <> " type>")

type MatchStuckMsg :: Symbol -> [FieldPath] -> FieldPath -> ErrorMessage
type family MatchStuckMsg viewType avail path where
  MatchStuckMsg viewType avail field =
    UnknownMsg viewType field %
    AvailableColumns avail

type MatchStuck :: Symbol -> FieldPath -> [FieldPath] -> k
type family MatchStuck viewType field avail where
  MatchStuck viewType field avail =
    TypeError (MatchStuckMsg viewType avail field)

type CheckMatch :: Symbol -> FieldPath -> [FieldPath] -> Bool -> Bool
type family CheckMatch viewType vfield avail match where
  CheckMatch viewType vfield avail match =
    IfStuck match (IfStuck (CheckAvail avail) (DelayError (AvailStuckMsg viewType vfield)) (Pure (MatchStuck viewType vfield avail))) (Pure match)

type CheckPathMatch :: FieldPath -> [FieldPath] -> Bool -> Bool
type family CheckPathMatch vfield avail match where
  CheckPathMatch vfield avail 'False =
    MatchViewPath vfield avail
  CheckPathMatch _ _ 'True =
    'True

type MatchViewPath :: FieldPath -> [FieldPath] -> Bool
type family MatchViewPath vfield avail where
  MatchViewPath vfield (tfield : tfields) =
    CheckPathMatch vfield tfields (PathEq vfield tfield)
  MatchViewPath _ '[] =
    'False

type MatchViewPathError :: Symbol -> Dd -> FieldPath -> [FieldPath] -> Maybe ErrorMessage
type family MatchViewPathError viewType table vfield avail where
  MatchViewPathError viewType table vfield avail =
    If (CheckMatch viewType vfield avail (MatchViewPath vfield avail))
    'Nothing
    ('Just (NoViewMatchTable table avail))

type CheckViewPathError :: Symbol -> Dd -> FieldPath -> [FieldPath] -> [FieldPath] -> Bool -> Maybe ErrorMessage
type family CheckViewPathError viewType table vfield vfields avail match where
  CheckViewPathError viewType table _ vfields avail 'True =
    MatchViewPaths viewType table vfields avail
  CheckViewPathError viewType _ vfield _ avail 'False =
    'Just (NoViewMatch viewType avail vfield)

type MatchViewPaths :: Symbol -> Dd -> [FieldPath] -> [FieldPath] -> Maybe ErrorMessage
type family MatchViewPaths viewType table view avail where
  MatchViewPaths viewType table (vfield : vfields) avail =
    CheckViewPathError viewType table vfield vfields avail
    (CheckMatch viewType vfield avail (MatchViewPath vfield avail))
  MatchViewPaths _ _ '[] _ =
    'Nothing

type MatchView :: Symbol -> Dd -> Dd -> Maybe ErrorMessage
type family MatchView viewType view table where
  MatchView viewType view table =
    MatchViewPaths viewType table (FieldPaths view) (FieldPaths table)
