module Sqel.Kind.CheckPrim where

import Type.Errors (ErrorMessage)

import Sqel.Class.MatchView (ColumnMessage, MatchViewPathError)
import Sqel.Data.Dd (Dd)
import Sqel.Data.Sel (Paths (Paths))
import Sqel.Dd (DdTypeName)
import Sqel.Kind.Error (Unlines)
import Sqel.Kind.FieldPath (FieldPath (FieldPath), FieldPaths)

data TableFPaths ext = TableFPaths Symbol (Dd ext) [FieldPath]

type MkTableFPath :: Dd ext -> TableFPaths ext
type family MkTableFPath table where
  MkTableFPath table = 'TableFPaths (DdTypeName table) table (FieldPaths table)

type MkTableFPaths :: [Dd ext] -> [TableFPaths ext]
type family MkTableFPaths tables where
  MkTableFPaths '[] = '[]
  MkTableFPaths (table : tables) = MkTableFPath table : MkTableFPaths tables

-- TODO viewType
type CheckAgainst :: TableFPaths ext -> FieldPath -> Maybe ErrorMessage
type family CheckAgainst table s where
  CheckAgainst ('TableFPaths _ table paths) path =
    MatchViewPathError "query" table path paths

type CheckPrimCont :: [ErrorMessage] -> Maybe ErrorMessage -> TableFPaths ext -> [TableFPaths ext] -> FieldPath -> Symbol
type family CheckPrimCont errors error table tables s where
  CheckPrimCont errors ('Just error) _ tables s = CheckPrimFP (error : errors) tables s
  CheckPrimCont _ 'Nothing ('TableFPaths name _ _) _ _ = name

type CheckPrimFP :: [ErrorMessage] -> [TableFPaths ext] -> FieldPath -> Symbol
type family CheckPrimFP errors tables s where
  CheckPrimFP errors '[] path = TypeError (ColumnMessage "query" path % Unlines errors)
  CheckPrimFP errors (table : tables) s = CheckPrimCont errors (CheckAgainst table s) table tables s

type CheckPrim :: [TableFPaths ext] -> Type -> Paths -> Symbol
type family CheckPrim tables a path where
  CheckPrim tables a ('Paths _ _ path) =
    CheckPrimFP '[] tables ('FieldPath path a)
