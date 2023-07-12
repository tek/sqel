module Sqel.Normalize where

import Sqel.Data.Dd (Dd0, Dd1, Dd (Dd), Ext (Ext), Ext0 (Ext0), Struct1, Struct0, Struct (Comp, Prim))
import Sqel.Data.Name (Name (Name, NameAuto))
import Sqel.Data.Sel (Path (PathPrefix, PathSet, PathSkip), Paths (Paths, PathsRoot), Sel (Sel))
import Sqel.Kind.Error (ShowPath)

type NormalizeName :: [Symbol] -> Name -> Symbol
type family NormalizeName pre name where
  NormalizeName _ ('Name name) = name
  NormalizeName pre 'NameAuto = TypeError ("NameAuto in non-root-comp node at " <> ShowPath pre)

type SetPath :: ([Symbol], [Symbol]) -> Ext0 -> Ext
type family SetPath paths sel where
  SetPath '(dd, table) ('Ext0 ('Sel name _) mods) =
    'Ext ('Paths (NormalizeName dd name) dd table) mods

type UpdatePreWith :: ([Symbol], [Symbol]) -> Maybe Path -> Symbol -> ([Symbol], [Symbol])
type family UpdatePreWith paths sel name where
  UpdatePreWith '(dd, table) ('Just 'PathSkip) name = '(name : dd, table)
  UpdatePreWith '(dd, _) ('Just ('PathSet table)) name = '(name : dd, table)
  UpdatePreWith '(dd, _) ('Just ('PathPrefix pre)) name = '(name : dd, name : pre)
  UpdatePreWith '(dd, table) 'Nothing name = '(name : dd, name : table)

type UpdatePre :: ([Symbol], [Symbol]) -> Ext0 -> ([Symbol], [Symbol])
type family UpdatePre paths sel where
  UpdatePre '(dd, table) ('Ext0 ('Sel name mod) _) = UpdatePreWith '(dd, table) mod (NormalizeName dd name)

type ResolvePathsComp :: ([Symbol], [Symbol]) -> [Dd0] -> [Dd1]
type family ResolvePathsComp pre s where
  ResolvePathsComp _ '[] = '[]
  ResolvePathsComp pre (s : ss) = ResolvePaths pre s : ResolvePathsComp pre ss

type ResolvePathsStruct :: ([Symbol], [Symbol]) -> Struct0 -> Struct1
type family ResolvePathsStruct pre s where
  ResolvePathsStruct _ ('Prim p) = 'Prim p
  ResolvePathsStruct pre ('Comp tsel c i sub) = 'Comp tsel c i (ResolvePathsComp pre sub)

type ResolvePathsNode :: ([Symbol], [Symbol]) -> Dd0 -> Dd1
type family ResolvePathsNode pre s where
  ResolvePathsNode pre ('Dd ext a s) =
    'Dd (SetPath pre ext) a (ResolvePathsStruct pre s)

type family InitPre (path :: Maybe Path) :: [Symbol] where
  InitPre 'Nothing = '[]
  InitPre ('Just 'PathSkip) = '[]
  InitPre ('Just ('PathSet pre)) = pre
  InitPre ('Just ('PathPrefix pre)) = pre

type ResolvePaths :: ([Symbol], [Symbol]) -> Dd0 -> Dd1
type family ResolvePaths pre s where
  ResolvePaths '( '[], '[]) ('Dd ('Ext0 ('Sel 'NameAuto path) mods) a ('Comp tsel c i sub)) =
    'Dd ('Ext 'PathsRoot mods) a ('Comp tsel c i (ResolvePathsComp '( '[], (InitPre path)) sub))
  ResolvePaths pre ('Dd ext a s) =
    ResolvePathsNode (UpdatePre pre ext) ('Dd ext a s)

type NormalizeDd :: Dd0 -> Dd1
type family NormalizeDd s where
  NormalizeDd s = ResolvePaths '( '[], '[]) s
