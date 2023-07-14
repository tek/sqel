module Sqel.Class.MatchPrim where

import Fcf (If)
import Type.Errors (DelayError, ErrorMessage, IfStuck, Pure)

import Sqel.Data.Dd (Dd)
import Sqel.Dd (DdType)
import Sqel.Kind.Error (Quoted, ShowPath, Unlines)
import Sqel.Kind.FieldPath (FieldPath (FieldPath), PathEq, ShowFields)

type NoFieldMatch :: FieldPath -> ErrorMessage
type family NoFieldMatch path where
  NoFieldMatch ('FieldPath path tpe) =
    "The column " <> ShowPath path <> " with type " <> Quoted tpe <>
    " does not correspond to a table column."

type CheckAvail :: [FieldPath] -> Type
type family CheckAvail fields where
  CheckAvail ('FieldPath _ t : _) = t

type AvailableColumns :: [FieldPath] -> ErrorMessage
type family AvailableColumns fields where
  AvailableColumns fields =
    "The specified table contains these fields:" %
    Unlines (ShowFields fields)

type NoViewMatch :: [FieldPath] -> FieldPath -> ErrorMessage
type family NoViewMatch avail path where
  NoViewMatch avail path =
    NoFieldMatch path %
    AvailableColumns avail

type NoViewMatchTable :: Dd ext -> [FieldPath] -> ErrorMessage
type family NoViewMatchTable table avail where
  NoViewMatchTable table avail =
    "The table for type " <> Quoted (DdType table) <> " contains these fields:" %
    Unlines (ShowFields avail)

type UnknownMsg :: FieldPath -> ErrorMessage
type family UnknownMsg field where
  UnknownMsg ('FieldPath path tpe) =
    "Cannot determine whether the column " <> ShowPath path <> " with type " <>
    Quoted tpe %
    "corresponds to a table column."

type PathConstraint :: [Symbol] -> ErrorMessage
type family PathConstraint path where
  PathConstraint '[field] = "HasColumn " <> 'ShowType field
  PathConstraint path = "HasPath " <> path

type AvailStuckMsg :: FieldPath -> ErrorMessage
type family AvailStuckMsg field where
  AvailStuckMsg ('FieldPath path tpe) =
    UnknownMsg ('FieldPath path tpe) %
    "This is likely because the table is polymorphic." %
    "Try adding the constraint:" %
    "  " <> Quoted "Check tables a" %
    "where " <> Quoted "tables" <> " is the list of tables and " <> Quoted "a" <> " is the query or projection." %
    "If there is only one table, you can use:" %
    "  " <> Quoted "Check1 table a"

type MatchStuckMsg :: [FieldPath] -> FieldPath -> ErrorMessage
type family MatchStuckMsg avail path where
  MatchStuckMsg avail field =
    UnknownMsg field %
    AvailableColumns avail

type MatchStuck :: FieldPath -> [FieldPath] -> k
type family MatchStuck field avail where
  MatchStuck field avail =
    TypeError (MatchStuckMsg avail field)

type CheckMatch :: FieldPath -> [FieldPath] -> Bool -> Bool
type family CheckMatch field avail match where
  CheckMatch field avail match =
    IfStuck match
    (IfStuck (CheckAvail avail) (DelayError (AvailStuckMsg field)) (Pure (MatchStuck field avail)))
    (Pure match)

type CheckPathMatch :: FieldPath -> [FieldPath] -> Bool -> Bool
type family CheckPathMatch field avail match where
  CheckPathMatch field avail 'False =
    MatchPrimPath field avail
  CheckPathMatch _ _ 'True =
    'True

type MatchPrimPath :: FieldPath -> [FieldPath] -> Bool
type family MatchPrimPath field avail where
  MatchPrimPath field (tfield : tfields) =
    CheckPathMatch field tfields (PathEq field tfield)
  MatchPrimPath _ '[] =
    'False

type MatchPrimPathError :: Dd ext -> FieldPath -> [FieldPath] -> Maybe ErrorMessage
type family MatchPrimPathError table field avail where
  MatchPrimPathError table field avail =
    If (CheckMatch field avail (MatchPrimPath field avail))
    'Nothing
    ('Just (NoViewMatchTable table avail))
