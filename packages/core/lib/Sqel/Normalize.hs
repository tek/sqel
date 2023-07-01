module Sqel.Normalize where

import Fcf (Fst, type (@@))

import Sqel.Class.Mods (HasMod)
import Sqel.Data.Dd (Dd, Dd0, DdK (Dd), Ext (Ext), Ext0 (Ext0), Sort (Sum), Struct, Struct0, StructWith (Comp, Prim))
import qualified Sqel.Data.Mods.Ignore as Mods
import Sqel.Data.Name (Name (Name, NameAuto))
import Sqel.Data.Sel (IxPaths (IxPaths), Path (Path, PathSkip), Paths (Paths, PathsRoot), Sel (Sel))
import Sqel.Dd (ExtMods)
import Sqel.SOP.Error (ShowPath)

-- TODO query indexes are set on every prim now since they can't be used manually anymore.
-- this suggests that paths and indexes can be set in one pass without a Maybe, though it would require composites to
-- either have an index (like that of its first field) or have a different ext type – maybe it should be a sum.
-- Note: turns out comps already get that index.
-- also, ignored fields don't work well with removing the Maybe.

type NormalizeName :: [Symbol] -> Name -> Symbol
type family NormalizeName pre name where
  NormalizeName _ ('Name name) = name
  NormalizeName pre 'NameAuto = TypeError ("NameAuto in non-root non-comp node at " <> ShowPath pre)

type SetPath :: ([Symbol], [Symbol]) -> Ext0 -> Ext
type family SetPath paths sel where
  SetPath '(dd, table) ('Ext0 ('Sel name _) mods) =
    'Ext ('IxPaths ('Paths (NormalizeName dd name) dd table) 'Nothing) mods

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
    'Dd ('Ext ('IxPaths 'PathsRoot 'Nothing) mods) a ('Comp tsel c i (ResolvePathsComp '( '[], '[]) sub))
  ResolvePaths pre ('Dd ext a s) =
    ResolvePathsNode (UpdatePre pre ext) ('Dd ext a s)

type QueryIndexesCompRec' :: Dd -> ([Dd], Nat) -> ([Dd], Nat)
type family QueryIndexesCompRec' acc query where
  QueryIndexesCompRec' s '(ss, cur) = '(s : ss, cur)

type QueryIndexesCompRec :: (Dd, Nat) -> [Dd] -> ([Dd], Nat)
type family QueryIndexesCompRec cur query where
  QueryIndexesCompRec '(s, cur) ss = QueryIndexesCompRec' s (QueryIndexesComp cur ss)

type QueryIndexesComp :: Nat -> [Dd] -> ([Dd], Nat)
type family QueryIndexesComp cur query where
  QueryIndexesComp cur '[] = '( '[], cur)
  QueryIndexesComp cur (s : ss) = QueryIndexesCompRec (QueryIndexes cur s) ss

type SetQueryIndexExt :: Nat -> Ext -> Ext
type family SetQueryIndexExt index ext where
  SetQueryIndexExt index ('Ext ('IxPaths path _) mods) = 'Ext ('IxPaths path ('Just index)) mods

type SetQueryIndex :: Nat -> Bool -> Dd -> (Dd, Nat)
type family SetQueryIndex index ignore s where
  SetQueryIndex index 'False ('Dd ext a s) = '( 'Dd (SetQueryIndexExt index ext) a s, index + 1)
  SetQueryIndex index 'True s = '(s, index)

type QueryIndexesRec :: ([Dd], Nat) -> Dd -> (Dd, Nat)
type family QueryIndexesRec acc query where
  QueryIndexesRec '(sub, cur) ('Dd ext a ('Comp tsel c i _)) = '( 'Dd ext a ('Comp tsel c i sub), cur)

type QueryIndexes :: Nat -> Dd -> (Dd, Nat)
type family QueryIndexes acc query where
  QueryIndexes cur ('Dd ext a ('Prim p)) =
    SetQueryIndex cur (HasMod Mods.Ignore (ExtMods ext)) ('Dd ext a ('Prim p))
  QueryIndexes cur ('Dd ext a ('Comp tsel ('Sum index) i sub)) =
    QueryIndexesRec (QueryIndexesComp (cur + 1) sub) ('Dd (SetQueryIndexExt cur ext) a ('Comp tsel ('Sum index) i sub))
  QueryIndexes cur ('Dd ext a ('Comp tsel c i sub)) =
    QueryIndexesRec (QueryIndexesComp cur sub) ('Dd ext a ('Comp tsel c i sub))

type QueryIndexesRoot :: Dd -> Dd
type family QueryIndexesRoot query where
  QueryIndexesRoot query = Fst @@ QueryIndexes 1 query

type NormalizeDd :: Dd0 -> Dd
type family NormalizeDd s where
  NormalizeDd s = ResolvePaths '( '[], '[]) s

type NormalizeQueryDd :: Dd0 -> Dd
type family NormalizeQueryDd s where
  NormalizeQueryDd s = QueryIndexesRoot (NormalizeDd s)
