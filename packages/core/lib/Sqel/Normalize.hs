module Sqel.Normalize where

import Sqel.Data.Dd (Dd, Dd0, DdK (Dd), Ext (Ext), Ext0 (Ext0), Struct, Struct0, StructWith (Comp, Prim))
import Sqel.Data.Name (Name (Name, NameAuto))
import Sqel.Data.Sel (Path (Path, PathSkip), Paths (Paths, PathsRoot), Sel (Sel))
import Sqel.Kind.Error (ShowPath)

type NormalizeName :: [Symbol] -> Name -> Symbol
type family NormalizeName pre name where
  NormalizeName _ ('Name name) = name
  NormalizeName pre 'NameAuto = TypeError ("NameAuto in non-root non-comp node at " <> ShowPath pre)

type SetPath :: ([Symbol], [Symbol]) -> Ext0 -> Ext
type family SetPath paths sel where
  SetPath '(dd, table) ('Ext0 ('Sel name _) mods) =
    'Ext ('Paths (NormalizeName dd name) dd table) mods

type UpdatePreWith :: ([Symbol], [Symbol]) -> Maybe Path -> Symbol -> ([Symbol], [Symbol])
type family UpdatePreWith paths sel name where
  UpdatePreWith '(dd, table) ('Just 'PathSkip) name = '(name : dd, table)
  UpdatePreWith '(dd, _) ('Just ('Path 'Nothing table)) name = '(name : dd, table)
  UpdatePreWith _ ('Just ('Path ('Just dd) table)) _ = '(dd, table)
  UpdatePreWith '(dd, table) 'Nothing name = '(name : dd, name : table)

type UpdatePre :: ([Symbol], [Symbol]) -> Ext0 -> ([Symbol], [Symbol])
type family UpdatePre paths sel where
  UpdatePre '(dd, table) ('Ext0 ('Sel name mod) _) = UpdatePreWith '(dd, table) mod (NormalizeName dd name)

type ResolvePathsComp :: ([Symbol], [Symbol]) -> [Dd0] -> [Dd]
type family ResolvePathsComp pre s where
  ResolvePathsComp _ '[] = '[]
  ResolvePathsComp pre (s : ss) = ResolvePaths pre s : ResolvePathsComp pre ss

type ResolvePathsStruct :: ([Symbol], [Symbol]) -> Struct0 -> Struct
type family ResolvePathsStruct pre s where
  ResolvePathsStruct _ ('Prim p) = 'Prim p
  ResolvePathsStruct pre ('Comp tsel c i sub) = 'Comp tsel c i (ResolvePathsComp pre sub)

type ResolvePathsNode :: ([Symbol], [Symbol]) -> Dd0 -> Dd
type family ResolvePathsNode pre s where
  ResolvePathsNode pre ('Dd ext a s) =
    'Dd (SetPath pre ext) a (ResolvePathsStruct pre s)

type ResolvePaths :: ([Symbol], [Symbol]) -> Dd0 -> Dd
type family ResolvePaths pre s where
  ResolvePaths '( '[], '[]) ('Dd ('Ext0 ('Sel 'NameAuto 'Nothing) mods) a ('Comp tsel c i sub)) =
    'Dd ('Ext 'PathsRoot mods) a ('Comp tsel c i (ResolvePathsComp '( '[], '[]) sub))
  ResolvePaths pre ('Dd ext a s) =
    ResolvePathsNode (UpdatePre pre ext) ('Dd ext a s)

type NormalizeDd :: Dd0 -> Dd
type family NormalizeDd s where
  NormalizeDd s = ResolvePaths '( '[], '[]) s
