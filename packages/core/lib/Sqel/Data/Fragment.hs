module Sqel.Data.Fragment where

import Exon (exon)
import GHC.Records (HasField (getField))

import Sqel.Data.Dd (Dd)
import Sqel.Data.Spine (SpineSort)
import Sqel.Data.Sqel (SqelFor)
import Sqel.Dd (DdType, IsComp)

type Frag0 :: Type -> Type
data Frag0 ext =
  Frag0 {
    tag :: Type,
    sort :: SpineSort,
    s :: Dd ext,
    root :: Bool,
    comp :: Bool
  }

type FragType :: Frag0 ext -> Type
type family FragType frag where
  FragType ('Frag0 _ _ s _ _) = DdType s

data FragOperand ext =
  FragOpLit Type
  |
  FragOpFrag (Frag0 ext)

type Frag :: Type -> Type
data Frag ext =
  Frag (Frag0 ext)
  |
  FragOp (FragOperand ext) (FragOperand ext)
  |
  FragLit Type

type FragmentOperand :: ∀ ext . FragOperand ext -> Type
data FragmentOperand k where
  FragmentOpLit :: a -> FragmentOperand ('FragOpLit a)
  FragmentOpFrag :: Fragment ('Frag k) -> FragmentOperand ('FragOpFrag k)

instance Show a => Show (FragmentOperand ('FragOpLit a)) where
  showsPrec d (FragmentOpLit a) =
    showParen (d > 10) [exon|FragmentOpLit #{showsPrec 11 a}|]

instance Show (Fragment ('Frag k)) => Show (FragmentOperand ('FragOpFrag k)) where
  showsPrec d (FragmentOpFrag frag) =
    showParen (d > 10) [exon|FragmentOpLit #{showsPrec 11 frag}|]

type Fragment :: ∀ ext . Frag ext -> Type
data Fragment k where
  Fragment :: SqelFor tag s -> Fragment ('Frag ('Frag0 tag sort s root comp))
  FragmentOp :: Text -> FragmentOperand l -> FragmentOperand r -> Fragment ('FragOp l r)
  FragmentLit :: a -> Fragment ('FragLit a)

instance (
    Show (SqelFor tag s)
  ) => Show (Fragment ('Frag ('Frag0 tag sort s root comp))) where
    showsPrec d (Fragment s) =
      showParen (d > 10) [exon|Fragment #{showsPrec 11 s}|]

instance (
    Show (FragmentOperand l),
    Show (FragmentOperand r)
  ) => Show (Fragment ('FragOp l r)) where
    showsPrec d (FragmentOp op l r) =
      showParen (d > 10) [exon|FragmentOp #{showsPrec 11 op} #{showsPrec 11 l} #{showsPrec 11 r}|]

instance Show a => Show (Fragment ('FragLit a)) where
  showsPrec d (FragmentLit a) =
    showParen (d > 10) [exon|FragmentLit #{showsPrec 11 a}|]

instance (
    HasField name (SqelFor tag s) (SqelFor tag s'),
    comp ~ IsComp s'
  ) => HasField name (Fragment ('Frag ('Frag0 tag sort s root comp0))) (Fragment ('Frag ('Frag0 tag sort s' 'False comp))) where
    getField (Fragment s) = Fragment (getField @name s)
