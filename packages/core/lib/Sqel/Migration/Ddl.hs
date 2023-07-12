module Sqel.Migration.Ddl where

import Fcf (Eval, Exp, type (@@))
import Fcf.Class.Functor (FMap)
import Fcf.Data.List (ConcatMap)

import Sqel.Class.Mods (FindMod, HasMod)
import Sqel.Constraints (ConstraintsFor)
import Sqel.Data.Constraints (ConstraintsK)
import Sqel.Data.Dd (Dd1, Dd (Dd), Ext (Ext), Inc (Merge, Nest), Sort (Sum), Struct1, Struct (Comp, Prim))
import Sqel.Data.Def (Def)
import Sqel.Data.Mods.MigrationDefault (MigrationDefault)
import Sqel.Data.Mods.MigrationDelete (MigrationDelete)
import Sqel.Data.Mods.MigrationRename (MigrationRename)
import Sqel.Data.Mods.MigrationRenameType (MigrationRenameType)
import Sqel.Data.Name (NamePrefix)
import Sqel.Data.Sel (Paths, PathsNameOr, RenderTSel, TSelName)
import Sqel.Dd (ExtMods, ExtName, ExtPath)
import Sqel.Kind.Error (Quoted)
import Sqel.Reify.PrimName (PrimName)

data DdlStruct =
  DdlPrim {
    def :: Maybe Symbol,
    colType :: Symbol
  }
  |
  DdlComp {
    table :: Bool,
    name :: Symbol,
    typeName :: Symbol,
    rename :: Maybe Symbol,
    indexPrefix :: Maybe NamePrefix,
    inc :: Inc
  }

data DdlConf =
  DdlConf {
    name :: Symbol,
    rename :: Maybe Symbol,
    delete :: Bool,
    constraints :: ConstraintsK,
    struct :: DdlStruct
  }

data ExtDdl =
  ExtDdl {
    path :: Paths,
    conf :: DdlConf,
    mods :: [Type]
  }

type instance ExtName @ExtDdl ('ExtDdl path _ _) = PathsNameOr "<root>" path

type instance ExtMods @ExtDdl ('ExtDdl _ _ mods) = mods

type instance ExtPath @ExtDdl ('ExtDdl path _ _) = path

type StructDdl = Struct ExtDdl

type Ddl = Dd ExtDdl

type GetDdlConf :: ∀ k . Bool -> k -> DdlConf
type family GetDdlConf table mods

type instance GetDdlConf @ExtDdl _ ('ExtDdl _ conf _) = conf

type instance GetDdlConf @(Dd ExtDdl) table ('Dd ext _ _) = GetDdlConf table ext

type instance GetDdlConf @(Dd Ext) table s = GetDdlConf table (Column table s)

type GetDdlMods :: [k] -> [DdlConf]
type family GetDdlMods s where
  GetDdlMods '[] = '[]
  GetDdlMods (s : ss) = GetDdlConf 'False s : GetDdlMods ss

type GetDdlSub :: Dd ext -> [DdlConf]
type family GetDdlSub s where
  GetDdlSub ('Dd ext _ ('Prim _)) = TypeError ("GetDdlDd called with prim named " <> Quoted (ExtName ext))
  GetDdlSub ('Dd _ _ ('Comp _ _ _ sub)) = GetDdlMods sub

type MaybeIndexPrefix :: Sort -> Maybe NamePrefix
type family MaybeIndexPrefix sort where
  MaybeIndexPrefix ('Sum prefix) = 'Just prefix
  MaybeIndexPrefix _ = 'Nothing

type StructConf :: Bool -> [Type] -> Type -> Struct1 -> DdlStruct
type family StructConf table mods a s where
  StructConf _ mods a ('Prim _) =
    'DdlPrim (FindMod MigrationDefault mods) (PrimName Def a mods)
  StructConf table mods _ ('Comp tsel sort inc _) =
    'DdlComp table (TSelName tsel) (RenderTSel table tsel) (FindMod MigrationRenameType mods) (MaybeIndexPrefix sort) inc

-- TODO strip ddl mods
type ColumnConf :: Paths -> Bool -> [Type] -> Type -> Struct1 -> ExtDdl
type family ColumnConf path table mods a s where
  ColumnConf path table mods a s =
    'ExtDdl path ('DdlConf (PathsNameOr "<root>" path) (FindMod MigrationRename mods) (HasMod MigrationDelete mods) (ConstraintsFor mods) (StructConf table mods a s)) mods

type MkStruct :: Struct1 -> StructDdl
type family MkStruct s where
  MkStruct ('Prim prim) = 'Prim prim
  MkStruct ('Comp tsel c i sub) = 'Comp tsel c i (FMap Sub @@ sub)

type Column :: Bool -> Dd1 -> Ddl
type family Column table s where
  Column table ('Dd ('Ext path mods) a s) =
    'Dd (ColumnConf path table mods a s) a (MkStruct s)

type ToDdl :: Dd1 -> Ddl
type family ToDdl s where
  ToDdl s = Column 'True s

type TypeToDdl :: Dd1 -> Ddl
type family TypeToDdl s where
  TypeToDdl s = Column 'False s

data Sub :: Dd1 -> Exp Ddl
type instance Eval (Sub s) = Column 'False s

type TypeList :: Ddl -> Ddl -> [Ddl]
type family TypeList s s' where
  TypeList _ ('Dd _ _ ('Prim _)) = '[]
  TypeList s ('Dd _ _ ('Comp _ _ 'Nest sub)) = s : ConcatMap TypeListF @@ sub
  TypeList _ ('Dd _ _ ('Comp _ _ 'Merge sub)) = ConcatMap TypeListF @@ sub

data TypeListF :: Ddl -> Exp [Ddl]
type instance Eval (TypeListF s) = TypeList s s

type DdDdlTypes :: Dd1 -> [Ddl]
type family DdDdlTypes s where
  DdDdlTypes s = TypeListF @@ ToDdl s
