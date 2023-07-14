module Sqel.Dd where

import Fcf (Eval, Exp, TyEq, type (@@))
import Fcf.Class.Functor (FMap)
import Fcf.Data.List (Concat)
import Prelude hiding (Compose)
import Type.Errors (ErrorMessage (Text))

import Sqel.Class.Mods (FindMod)
import Sqel.Data.Dd (
  Dd (Dd),
  Dd0,
  Ext (Ext),
  Ext0 (Ext0),
  Inc (Merge, Nest),
  Sort (Con, Prod, Sum),
  Struct (Comp, Prim),
  )
import Sqel.Data.Mods (NoMods)
import Sqel.Data.Mods.TableName (TableName)
import Sqel.Data.Name (Name (Name), NamePrefix (DefaultPrefix), SetName)
import Sqel.Data.Sel (
  Path (PathPrefix, PathSet),
  Paths (Paths, PathsRoot),
  PathsName,
  PathsNameOr,
  Sel (Sel),
  ShowSel,
  TSel (TSel),
  TSelName,
  )
import Sqel.Kind.Error (Quoted, ShowPath, Unlines)
import Sqel.Kind.List (type (++))

type MapSub :: (o -> Exp ext) -> [Dd o] -> [Dd ext]
type family MapSub f s where
  MapSub _ '[] = '[]
  MapSub f (s : ss) = MapDdK f s : MapSub f ss

type MapStruct :: (o -> Exp ext) -> Struct o -> Struct ext
type family MapStruct f s where
  MapStruct _ ('Prim p) = 'Prim p
  MapStruct f ('Comp sel c i sub) = 'Comp sel c i (MapSub f sub)

type StructTypeName :: Struct ext -> Maybe Symbol
type family StructTypeName s where
  StructTypeName ('Prim _) = 'Nothing
  StructTypeName ('Comp tsel _ _ _) = 'Just (TSelName tsel)

type MapDdK :: (o -> Exp ext) -> Dd o -> Dd ext
type family MapDdK f s where
  MapDdK f ('Dd o t s) = 'Dd (Eval (f o)) t (MapStruct f s)

type DdName :: Dd ext -> Symbol
type family DdName s

type instance DdName ('Dd ('Ext ('Paths name _ _) _) _ _) = name
type instance DdName ('Dd ('Ext0 ('Sel ('Name name) _) _) _ _) = name

data DdNameF :: Dd ext -> Exp Symbol
type instance Eval (DdNameF s) = DdName s

type DdNames :: [Dd ext] -> [Symbol]
type family DdNames s where
  DdNames '[] = '[]
  DdNames (s : ss) = DdName s : DdNames ss

type family DdNamesMerged ss :: [Symbol] where
  DdNamesMerged '[] = '[]
  DdNamesMerged ('Dd ext _ ('Comp _ _ 'Merge sub) : ss) = ExtName ext : DdNamesMerged sub ++ DdNamesMerged ss
  DdNamesMerged (s : ss) = DdName s : DdNamesMerged ss

type DdType :: ∀ {ext} . Dd ext -> Type
type family DdType s where
  DdType ('Dd _ a _) = a

type DdTypes :: ∀ {ext} . [Dd ext] -> [Type]
type family DdTypes s where
  DdTypes '[] = '[]
  DdTypes (s : ss) = DdType s : DdTypes ss

type MaybeDdType :: ∀ {ext} . Maybe (Dd ext) -> Type
type family MaybeDdType s where
  MaybeDdType 'Nothing = ()
  MaybeDdType ('Just s) = DdType s

type DdX :: Dd ext -> ext
type family DdX s where
  DdX ('Dd ext _ _) = ext

type DdTypeSel :: ∀ {ext} . Dd ext -> TSel
type family DdTypeSel s where
  DdTypeSel ('Dd _ _ ('Comp sel _ _ _)) = sel

type DdStruct :: Dd ext -> Struct ext
type family DdStruct s where
  DdStruct ('Dd _ _ s) = s

type StructSub :: Struct ext -> [Dd ext]
type family StructSub s where
  StructSub ('Prim _) = '[]
  StructSub ('Comp _ _ _ sub) = sub

type DdSub :: Dd ext -> [Dd ext]
type family DdSub s where
  DdSub ('Dd _ _ ('Prim _)) = '[]
  DdSub ('Dd _ _ ('Comp _ _ _ sub)) = sub

type DdInc :: Dd ext -> Inc
type family DdInc s where
  DdInc ('Dd _ _ ('Comp _ _ inc _)) = inc

------------------------------------------------------------------------------------------------------------------------

type EmptyProd ext =
  'Dd ext () ('Comp ('TSel 'DefaultPrefix "()") 'Prod 'Nest '[])

type EmptyQuery =
  EmptyProd ('Ext 'PathsRoot NoMods)

type EmptyResult =
  EmptyProd ('Ext 'PathsRoot NoMods)

------------------------------------------------------------------------------------------------------------------------

type ShowPathShort :: ∀ k . k -> ErrorMessage
type family ShowPathShort path where
  ShowPathShort @Paths ('Paths _ path _) = " " <> ShowPath path
  ShowPathShort @Paths 'PathsRoot = 'Text ""
  ShowPathShort @Ext0 ('Ext0 sel _) = ShowPathShort sel
  ShowPathShort @Ext ('Ext sel _) = ShowPathShort sel
  ShowPathShort path = " (" <> path <> ")"

type DescribeSort :: Sort -> Symbol
type family DescribeSort c where
  DescribeSort 'Prod = "Prod"
  DescribeSort 'Con = "Con"
  DescribeSort ('Sum _) = "Sum"

data ShowDdLinesL :: ErrorMessage -> Dd ext -> Exp [ErrorMessage]

type instance Eval (ShowDdLinesL indent ('Dd ext t ('Prim _))) =
  '[indent <> "* Prim " <> Quoted t <> ShowPathShort ext]

type instance Eval (ShowDdLinesL indent ('Dd ext t ('Comp _ c _ sub))) =
  (indent <> "- " <> DescribeSort c <> " " <> Quoted t <> ShowPathShort ext) :
  Concat @@ (FMap (ShowDdLinesL (indent <> "  ")) @@ sub)

type ShowDdLines :: Dd ext -> ErrorMessage
type family ShowDdLines dd where
  ShowDdLines dd = Unlines (Eval (ShowDdLinesL ('Text "") dd))

------------------------------------------------------------------------------------------------------------------------

type IsComp :: Dd ext -> Bool
type family IsComp s where
  IsComp ('Dd _ _ ('Prim _)) = 'False
  IsComp ('Dd _ _ ('Comp _ _ _ _)) = 'True

------------------------------------------------------------------------------------------------------------------------

type ExtName :: ∀ k . k -> Symbol
type family ExtName ext

type instance ExtName @Ext0 ('Ext0 sel _) = ShowSel sel
type instance ExtName @Ext ('Ext path _) = PathsNameOr "<root>" path

type ExtPath :: ∀ k . k -> Paths
type family ExtPath ext

type instance ExtPath @Ext ('Ext path _) = path

type ExtMods :: ∀ k . k -> [Type]
type family ExtMods ext

type instance ExtMods @Ext0 ('Ext0 _ mods) = mods
type instance ExtMods @Ext ('Ext _ mods) = mods
type instance ExtMods @(Dd _) ('Dd ext _ _) = ExtMods ext

------------------------------------------------------------------------------------------------------------------------

type DdTypeName :: Dd ext -> Symbol
type family DdTypeName s where
  DdTypeName ('Dd _ _ ('Comp tsel _ _ _)) = TSelName tsel
  DdTypeName ('Dd _ a _) = TypeError ("This Dd for type " <> a <> " has no type name")

type DdTypeNames :: [Dd ext] -> [Symbol]
type family DdTypeNames s where
  DdTypeNames '[] = '[]
  DdTypeNames (s : ss) = DdTypeName s : DdTypeNames ss

type ExtHasName :: Symbol -> ext -> Bool
type ExtHasName name ext = TyEq name @@ ExtName ext

type DdHasName :: Symbol -> Dd ext -> Bool
type DdHasName name s = TyEq name @@ DdName s

type ExtTableName :: ∀ ext . Dd ext -> Symbol
type family ExtTableName s where
  ExtTableName ('Dd _ _ ('Comp tsel _ _ _)) = TSelName tsel
  ExtTableName @Ext0 ('Dd ('Ext0 ('Sel ('Name name) _) _) _ ('Prim _)) = name
  ExtTableName @Ext ('Dd ('Ext path _) _ ('Prim _)) = PathsName path

type EffectiveTableName :: ∀ ext . Maybe Symbol -> Dd ext -> Symbol
type family EffectiveTableName mod s where
  EffectiveTableName 'Nothing s = ExtTableName s
  EffectiveTableName ('Just name) _ = name

type DdTableName :: Dd ext -> Symbol
type family DdTableName s where
  DdTableName s = EffectiveTableName (FindMod TableName (ExtMods s)) s

type SetDdName :: Symbol -> Dd0 -> Dd0
type family SetDdName name s where
  SetDdName name ('Dd ('Ext0 ('Sel old path) mods) a s) =
    'Dd ('Ext0 ('Sel (SetName name old) path) mods) a s

type SetDdPath :: [Symbol] -> Dd0 -> Dd0
type family SetDdPath path s where
  SetDdPath path ('Dd ('Ext0 ('Sel name _) mods) a s) =
    'Dd ('Ext0 ('Sel name ('Just ('PathSet path))) mods) a s

type SetDdPrefix :: [Symbol] -> Dd0 -> Dd0
type family SetDdPrefix pre s where
  SetDdPrefix pre ('Dd ('Ext0 ('Sel name _) mods) a s) =
    'Dd ('Ext0 ('Sel name ('Just ('PathPrefix pre))) mods) a s
