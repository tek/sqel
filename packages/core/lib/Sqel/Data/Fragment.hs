module Sqel.Data.Fragment where

import Exon (exon)
import GHC.Records (HasField (getField))

import Sqel.Data.Dd (DdK)
import Sqel.Data.Spine (SpineSort)
import Sqel.Data.Sqel (SqelFor)
import Sqel.Dd (IsComp)

type Frag0 :: Type -> Type
data Frag0 ext =
  Frag0 {
    tag :: Type,
    sort :: SpineSort,
    s :: DdK ext,
    root :: Bool,
    comp :: Bool
  }

type Frag :: Type -> Type
data Frag ext =
  Frag (Frag0 ext)
  |
  FragOp (Frag0 ext) (Frag0 ext)

type Fragment :: Frag ext -> Type
data Fragment k where
  Fragment :: SqelFor tag s -> Fragment ('Frag ('Frag0 tag sort s root comp))
  FragmentOp :: Text -> Fragment ('Frag l) -> Fragment ('Frag r) -> Fragment ('FragOp l r)

instance (
    Show (SqelFor tag s)
  ) => Show (Fragment ('Frag ('Frag0 tag sort s root comp))) where
    showsPrec d (Fragment s) =
      showParen (d > 10) [exon|Fragment #{showsPrec 11 s}|]

instance (
    Show (Fragment ('Frag l)),
    Show (Fragment ('Frag r))
  ) => Show (Fragment ('FragOp l r)) where
    showsPrec d (FragmentOp op l r) =
      showParen (d > 10) [exon|FragmentOp #{showsPrec 11 op} #{showsPrec 11 l} #{showsPrec 11 r}|]

instance (
    HasField name (SqelFor tag s) (SqelFor tag s'),
    comp ~ IsComp s'
  ) => HasField name (Fragment ('Frag ('Frag0 tag sort s root comp0))) (Fragment ('Frag ('Frag0 tag sort s' 'False comp))) where
    getField (Fragment s) = Fragment (getField @name s)
