module Sqel.Data.Sel where

import Type.Errors (ErrorMessage (Text))

import Sqel.Data.Name (IndexPrefixed, Name (Name, NameAuto), NamePrefix (DefaultPrefix, NamePrefix, NoPrefix), ShowName)
import Sqel.Text.DbIdentifier (dbSymbol)

type IndexName :: NamePrefix -> Symbol -> Symbol -> Constraint
class KnownSymbol name => IndexName prefix tpe name | prefix tpe -> name where

instance (
    name ~ IndexPrefixed prefixSpec tpe,
    KnownSymbol name
  ) => IndexName prefixSpec tpe name where

type Prefixed :: NamePrefix -> Symbol -> Symbol -> Symbol
type family Prefixed spec def name where
  Prefixed 'DefaultPrefix def name = AppendSymbol def name
  Prefixed 'NoPrefix _ name = name
  Prefixed ('NamePrefix spec) _ name = AppendSymbol spec name

type PrefixedType :: NamePrefix -> Symbol -> Symbol
type PrefixedType spec name = Prefixed spec "sqel_type__" name

type PrefixedIndex :: NamePrefix -> Symbol -> Symbol
type PrefixedIndex spec name = Prefixed spec "sqel_sum_index__" name

type PrefixedIndexMaybe :: Maybe NamePrefix -> Symbol -> Symbol
type family PrefixedIndexMaybe spec name where
  PrefixedIndexMaybe ('Just spec) name = Prefixed spec "sqel_sum_index__" name
  PrefixedIndexMaybe 'Nothing name = Prefixed 'DefaultPrefix "sqel_sum_index__" name

type TypeName :: NamePrefix -> Symbol -> Symbol -> Constraint
class (
    KnownSymbol name,
    KnownSymbol tpe
  ) => TypeName prefix tpe name | prefix tpe -> name where

instance (
    name ~ PrefixedType prefixSpec tpe,
    KnownSymbol name,
    KnownSymbol tpe
  ) => TypeName prefixSpec tpe name where

-- TODO Skip should be in Comp, it doesn't make sense for Prim
-- TODO check whether Skip is still used
data Path =
  Path { dd :: Maybe [Symbol], table :: [Symbol] }
  |
  PathSkip

data Paths =
  PathsRoot
  |
  Paths {
    name :: Symbol,
    dd :: [Symbol],
    table :: [Symbol]
  }

type PathsNameOr :: Symbol -> Paths -> Symbol
type family PathsNameOr root path where
  PathsNameOr root 'PathsRoot = root
  PathsNameOr _ ('Paths name _ _) = name

type PathsOr :: [Symbol] -> Paths -> [Symbol]
type family PathsOr root path where
  PathsOr root 'PathsRoot = root
  PathsOr _ ('Paths _ _ table) = table

type PathsL :: Paths -> [Symbol]
type family PathsL path where
  PathsL path = PathsOr '[] path

type PathsName :: Paths -> Symbol
type family PathsName path where
  PathsName 'PathsRoot = TypeError ('Text "PathsName of root")
  PathsName ('Paths name _ _) = name

data Sel =
  Sel {
    name :: Name,
    mod :: Maybe Path
  }

type SelAuto = 'Sel 'NameAuto 'Nothing

type SelName name = 'Sel ('Name name) 'Nothing

type SelSkip name = 'Sel ('Name name) ('Just 'PathSkip)

type SelPath name table = 'Sel ('Name name) ('Just ('Path 'Nothing table))

type ShowSel :: Sel -> Symbol
type family ShowSel sel where
  ShowSel ('Sel name _) = ShowName name

data TSel =
  TSel NamePrefix Symbol

type TSelNamed :: Symbol -> TSel
type family TSelNamed name where
  TSelNamed name = 'TSel 'DefaultPrefix name

type TSelName :: TSel -> Symbol
type family TSelName sel where
  TSelName ('TSel _ name) = name

type TSelTypeName :: TSel -> Symbol
type family TSelTypeName sel where
  TSelTypeName ('TSel prefix name) = PrefixedType prefix name

type RenderTSel :: Bool -> TSel -> Symbol
type family RenderTSel table sel where
  RenderTSel 'True ('TSel 'DefaultPrefix name) = name
  RenderTSel 'False sel = TSelTypeName sel
  RenderTSel 'True sel = TypeError ("Can't render table TSel with prefix: " <> sel)

type TSelWithPrefix :: Symbol -> TSel -> TSel
type family TSelWithPrefix prefix tsel where
  TSelWithPrefix prefix ('TSel _ name) = 'TSel ('NamePrefix prefix) name

tselTypeName ::
  ∀ (s :: TSel) .
  KnownSymbol (TSelTypeName s) =>
  Text
tselTypeName =
  dbSymbol @(TSelTypeName s)

tselTableName ::
  ∀ (s :: TSel) .
  KnownSymbol (TSelName s) =>
  Text
tselTableName =
  dbSymbol @(TSelName s)
