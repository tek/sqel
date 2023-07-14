module Sqel.Data.Dd where

import Generics.SOP (I, NP)
import qualified Text.Show

import Sqel.Data.Name (NamePrefix)
import Sqel.Data.Sel (Paths, Sel, TSel)

type ConCol :: [Type] -> Type
newtype ConCol as = ConCol (NP I as)

type ConColF :: (Type -> Type) -> [Type] -> Type
newtype ConColF f as = ConColF { conColF :: f (NP I as) }

data Ext0 =
  Ext0 {
    sel :: Sel,
    mods :: [Type]
  }

data Ext =
  Ext {
    path :: Paths,
    mods :: [Type]
  }

data PrimType =
  Cond
  |
  NoCond

data Sort = Prod | Con | Sum NamePrefix

data Inc = Merge | Nest

type SInc :: Inc -> Type
data SInc i where
  SMerge :: SInc 'Merge
  SNest :: SInc 'Nest

instance Eq (SInc i) where
  SMerge == SMerge = True
  SNest == SNest = True

instance Show (SInc i) where
  show = \case
    SMerge -> "SMerge"
    SNest -> "SNest"

pattern AnyInc :: SInc i
pattern AnyInc <- _

type Struct :: Type -> Type
data Struct ext =
  Prim PrimType
  |
  Comp {
    typeName :: TSel,
    sort :: Sort,
    inc :: Inc,
    sub :: [Dd ext]
  }

type Struct0 = Struct Ext0

type Struct1 = Struct Ext

type Dd :: Type -> Type
data Dd ext =
  Dd {
    ext :: ext,
    hsType :: Type,
    struct :: Struct ext
  }

type Dd0 = Dd Ext0

type Dd1 = Dd Ext

-- TODO remove?
type (:>) :: Type -> Type -> Type
data a :> b = a :> b
infixr 3 :>
