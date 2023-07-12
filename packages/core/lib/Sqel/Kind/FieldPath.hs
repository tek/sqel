module Sqel.Kind.FieldPath where

import Type.Errors (ErrorMessage)

import Sqel.Data.Dd (Dd (Dd), Ext (Ext), PrimType (Cond, NoCond), Struct (Comp, Prim))
import Sqel.Data.Sel (Paths (Paths))
import Sqel.Kind.Error (QuotedType, ShowPath, Unlines)
import Sqel.Kind.List (type (++))

data FieldPath =
  FieldPath {
    path :: [Symbol],
    tpe :: Type
  }

type FieldPathPrim :: Paths -> Type -> [FieldPath]
type family FieldPathPrim sel t where
  FieldPathPrim ('Paths _ _ path) t = '[ 'FieldPath path t]

type FieldPathsProd :: [Dd ext] -> [FieldPath]
type family FieldPathsProd s where
  FieldPathsProd '[] = '[]
  FieldPathsProd (s : ss) = FieldPaths s ++ FieldPathsProd ss
  FieldPathsProd s = TypeError ("FieldPathsProd: " <> s)

type FieldPaths :: Dd ext -> [FieldPath]
type family FieldPaths s where
  FieldPaths ('Dd ('Ext path _) t ('Prim 'Cond)) = FieldPathPrim path t
  FieldPaths ('Dd _ _ ('Prim 'NoCond)) = '[]
  FieldPaths ('Dd _ _ ('Comp _ _ _ sub)) = FieldPathsProd sub
  FieldPaths s = TypeError ("FieldPathsSub: " <> s)

-------------------------------------------------------------------------------------------------------

type PathEq :: FieldPath -> FieldPath -> Bool
type family PathEq f1 f2 where
  PathEq ('FieldPath path _) ('FieldPath path _) = 'True
  PathEq _ _ = 'False

type ShowField :: FieldPath -> ErrorMessage
type family ShowField field where
  ShowField ('FieldPath path tpe) = "  " <> ShowPath path <> ": " <> QuotedType tpe

type ShowFields :: [FieldPath] -> [ErrorMessage]
type family ShowFields fields where
  ShowFields '[] = '[]
  ShowFields (field : fields) =
    ShowField field : ShowFields fields

class PrintFields (s :: Dd ext) where
  printFields :: ()
  printFields = ()

instance (
    TypeError (Unlines (ShowFields (FieldPaths s)) % s)
  ) => PrintFields s where
