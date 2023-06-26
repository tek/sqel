module Sqel.Class.AcceptFrag where

import Data.Type.Bool (type (&&))
import Generics.SOP (All, NP ((:*)))

import Sqel.Data.ClauseConfig (ClauseSortsFor)
import Sqel.Data.Dd (DdK (Dd), StructWith (Prim))
import Sqel.Data.Field (
  CondField (CondField, CondOp),
  Field (Field),
  PrimField (PrimField),
  RootField (RootField),
  TypeField (TypeField),
  )
import Sqel.Data.Fragment (Frag (Frag, FragOp), Frag0 (Frag0), Fragment (Fragment, FragmentOp))
import Sqel.Data.Spine (SpineSort (SpineTable))
import Sqel.Data.Sqel (SqelFor (SqelPrim), sqelSpine)
import Sqel.Error.Fragment (CheckFragmentMismatch)
import Sqel.SOP.NP (hcmapList)

type AcceptRoot :: Bool -> Type -> Bool
type family AcceptRoot root field where
  AcceptRoot 'False (RootField _) = 'False
  AcceptRoot _ _ = 'True

type AcceptSort :: SpineSort -> [SpineSort] -> Bool
type family AcceptSort sort accepted where
  AcceptSort _ '[] = 'False
  AcceptSort sort (sort : _) = 'True
  AcceptSort sort (_ : accepted) = AcceptSort sort accepted

------------------------------------------------------------------------------------------------------------------------

type DemoteFrag :: ∀ {ext} . Frag0 ext -> Type -> Constraint
class DemoteFrag frag field where
  demoteFrag :: Fragment ('Frag frag) -> field

instance DemoteFrag ('Frag0 tag sort s root comp) (Field tag) where
    demoteFrag (Fragment s) = Field (sqelSpine s)

instance DemoteFrag ('Frag0 tag sort ('Dd ext a ('Prim prim)) root comp) (PrimField tag) where
    demoteFrag (Fragment (SqelPrim meta _)) = PrimField meta

instance DemoteFrag ('Frag0 tag sort s root comp) (CondField tag) where
    demoteFrag (Fragment s) = CondField (sqelSpine s)

instance DemoteFrag ('Frag0 tag sort s 'True comp) (RootField tag) where
  demoteFrag (Fragment s) = RootField (sqelSpine s)

instance DemoteFrag ('Frag0 tag 'SpineTable s root 'True) (TypeField tag) where
  demoteFrag (Fragment s) = TypeField (sqelSpine s)

------------------------------------------------------------------------------------------------------------------------

type AcceptFrag :: ∀ {ext} . Bool -> Constraint -> Frag0 ext -> Type -> Constraint
class AcceptFrag accepted error frag field where
  acceptFrag :: Fragment ('Frag frag) -> field

instance DemoteFrag frag field => AcceptFrag 'True error frag field where
  acceptFrag = demoteFrag

------------------------------------------------------------------------------------------------------------------------

type AcceptFragOrError :: ∀ {ext} . Type -> Type -> Frag0 ext -> Constraint
class AcceptFragOrError clause field frag where
  acceptFragOrError :: Fragment ('Frag frag) -> field

instance (
    acceptSort ~ AcceptSort sort (ClauseSortsFor clause),
    acceptRoot ~ AcceptRoot root field,
    accepted ~ (acceptSort && acceptRoot),
    error ~ CheckFragmentMismatch tag clause sort acceptSort acceptRoot,
    AcceptFrag accepted error ('Frag0 tag sort spine root comp) field
  ) => AcceptFragOrError clause field ('Frag0 tag sort spine root comp) where
    acceptFragOrError = acceptFrag @accepted @error

------------------------------------------------------------------------------------------------------------------------

type AcceptFragment :: ∀ {ext} . Type -> Type -> Frag ext -> Constraint
class AcceptFragment clause field frag where
  acceptFragment :: Fragment frag -> field

instance (
    AcceptFragOrError clause field frag
  ) => AcceptFragment clause field ('Frag frag) where
    acceptFragment = acceptFragOrError @clause @field

instance (
    AcceptFragOrError clause (Field tag) l,
    AcceptFragOrError clause (Field tag) r
  ) => AcceptFragment clause (CondField tag) ('FragOp l r) where
    acceptFragment (FragmentOp op l r) =
      CondOp op lfrag rfrag
      where
        Field lfrag = acceptFragOrError @clause l
        Field rfrag = acceptFragOrError @clause r

------------------------------------------------------------------------------------------------------------------------

class AcceptFragments clause frags field f where
  acceptFragments :: NP Fragment frags -> f field

instance (
    All (AcceptFragment clause field) frags
  ) => AcceptFragments clause frags field [] where
    acceptFragments frags =
      hcmapList @(AcceptFragment clause field) (acceptFragment @clause) frags

instance (
    All (AcceptFragment clause field) (frag : frags)
  ) => AcceptFragments clause (frag : frags) field NonEmpty where
    acceptFragments (frag :* frags) =
      acceptFragment @clause frag :|
      hcmapList @(AcceptFragment clause field) (acceptFragment @clause) frags
