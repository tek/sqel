module Sqel.Class.AcceptFrag where

import Data.Type.Bool (type (&&), type (||))
import Exon (exon)
import Generics.SOP (All, NP ((:*)))

import Sqel.Data.ClauseConfig (ClauseLiteralFor, ClauseSortsFor)
import Sqel.Data.Dd (DdK (Dd), StructWith (Prim))
import Sqel.Data.Field (
  CondField (CondField, CondOp),
  CondOperand (CondOpField, CondOpLit),
  Field (Field),
  OrLiteral (LiteralField, NotLiteral),
  PrimField (PrimField),
  RootField (RootField),
  TypeField (TypeField),
  )
import Sqel.Data.Fragment (
  Frag (Frag, FragLit, FragOp),
  Frag0 (Frag0),
  FragOperand (FragOpFrag, FragOpLit),
  Fragment (Fragment, FragmentLit, FragmentOp),
  FragmentOperand (FragmentOpFrag, FragmentOpLit),
  )
import Sqel.Data.Spine (SpineSort (SpineTable))
import Sqel.Data.Sqel (SqelFor (SqelPrim), sqelSpine)
import Sqel.Data.Sql (Sql (Sql))
import Sqel.Error.Fragment (CheckFragmentMismatch, NoLiteralField)
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

type AcceptLiteral :: Type -> Maybe Type -> Bool
type family AcceptLiteral lit accepted where
  AcceptLiteral lit ('Just lit) = 'True
  AcceptLiteral _ _ = 'False

------------------------------------------------------------------------------------------------------------------------

type DemoteFragment :: ∀ {ext} . Frag0 ext -> Type -> Constraint
class DemoteFragment frag field where
  demoteFragment :: Fragment ('Frag frag) -> field

instance (
    DemoteFragment frag field
  ) => DemoteFragment frag (OrLiteral lit field) where
    demoteFragment frag = NotLiteral (demoteFragment frag)

instance DemoteFragment ('Frag0 tag sort s root comp) (Field tag) where
    demoteFragment (Fragment s) = Field (sqelSpine s)

instance DemoteFragment ('Frag0 tag sort ('Dd ext a ('Prim prim)) root comp) (PrimField tag) where
    demoteFragment (Fragment (SqelPrim meta _)) = PrimField meta

instance DemoteFragment ('Frag0 tag sort s root comp) (CondField tag) where
    demoteFragment (Fragment s) = CondField (sqelSpine s)

instance DemoteFragment ('Frag0 tag sort s 'True comp) (RootField tag) where
  demoteFragment (Fragment s) = RootField (sqelSpine s)

instance DemoteFragment ('Frag0 tag 'SpineTable s root 'True) (TypeField tag) where
  demoteFragment (Fragment s) = TypeField (sqelSpine s)

------------------------------------------------------------------------------------------------------------------------

type DemoteLiteral :: Type -> Constraint
class DemoteLiteral lit where
  demoteLiteral :: lit -> Sql

instance DemoteLiteral Int64 where
  demoteLiteral = Sql . show

instance DemoteLiteral Text where
  demoteLiteral s = Sql [exon|'#{s}'|]

instance DemoteLiteral Bool where
  demoteLiteral = \case
    True -> "true"
    False -> "false"

------------------------------------------------------------------------------------------------------------------------

type AcceptFrag :: ∀ {ext} . Bool -> Constraint -> Frag0 ext -> Type -> Constraint
class AcceptFrag accepted error frag field where
  acceptFrag :: Fragment ('Frag frag) -> field

instance DemoteFragment frag field => AcceptFrag 'True error frag field where
  acceptFrag = demoteFragment

------------------------------------------------------------------------------------------------------------------------

type DemoteLiteralField :: Type -> Type -> Constraint
class DemoteLiteralField lit field where
  demoteLiteralField :: lit -> field

instance DemoteLiteralField lit (OrLiteral lit field) where
  demoteLiteralField = LiteralField

------------------------------------------------------------------------------------------------------------------------

type AcceptFragLit :: Bool -> Constraint -> Type -> Type -> Constraint
class AcceptFragLit accepted error lit field where
  acceptFragLit :: lit -> field

instance DemoteLiteralField lit field => AcceptFragLit 'True error lit field where
  acceptFragLit = demoteLiteralField

------------------------------------------------------------------------------------------------------------------------

type AcceptFragOrError :: ∀ {ext} . Type -> Bool -> Type -> Frag0 ext -> Constraint
class AcceptFragOrError clause anySort field frag where
  acceptFragOrError :: Fragment ('Frag frag) -> field

instance (
    acceptSort ~ AcceptSort sort (ClauseSortsFor clause),
    acceptRoot ~ AcceptRoot root field,
    accepted ~ ((anySort || acceptSort) && acceptRoot),
    error ~ CheckFragmentMismatch tag clause sort acceptSort acceptRoot,
    AcceptFrag accepted error ('Frag0 tag sort spine root comp) field
  ) => AcceptFragOrError clause anySort field ('Frag0 tag sort spine root comp) where
    acceptFragOrError = acceptFrag @accepted @error

------------------------------------------------------------------------------------------------------------------------

type AcceptFragmentLiteral :: Type -> Type -> Type -> Constraint
class AcceptFragmentLiteral clause field lit where
  acceptFragmentLiteral :: Fragment ('FragLit lit) -> field

instance (
    accepted ~ AcceptLiteral lit (ClauseLiteralFor clause),
    error ~ NoLiteralField clause lit,
    AcceptFragLit accepted error lit field
  ) => AcceptFragmentLiteral clause field lit where
    acceptFragmentLiteral (FragmentLit lit) = acceptFragLit @accepted @error lit

------------------------------------------------------------------------------------------------------------------------

type AcceptFragmentOperand :: ∀ {ext} . Type -> Type -> FragOperand ext -> Constraint
class AcceptFragmentOperand clause field frag where
  acceptFragmentOperand :: FragmentOperand frag -> field

instance (
    DemoteLiteral lit
  ) => AcceptFragmentOperand clause (CondOperand tag) ('FragOpLit lit) where
    acceptFragmentOperand (FragmentOpLit lit) = CondOpLit (demoteLiteral lit)

instance (
    AcceptFragOrError clause 'True (Field tag) frag
  ) => AcceptFragmentOperand clause (CondOperand tag) ('FragOpFrag frag) where
    acceptFragmentOperand (FragmentOpFrag frag) =
      CondOpField field
      where
        Field field = acceptFragOrError @clause @'True frag

------------------------------------------------------------------------------------------------------------------------

type AcceptFragment :: ∀ {ext} . Type -> Type -> Frag ext -> Constraint
class AcceptFragment clause field frag where
  acceptFragment :: Fragment frag -> field

instance AcceptFragOrError clause 'False field frag => AcceptFragment clause field ('Frag frag) where
  acceptFragment = acceptFragOrError @clause @'False @field

instance (
    AcceptFragmentOperand clause (CondOperand tag) l,
    AcceptFragmentOperand clause (CondOperand tag) r
  ) => AcceptFragment clause (CondField tag) ('FragOp l r) where
    acceptFragment (FragmentOp op l r) =
      CondOp op lfrag rfrag
      where
        lfrag = acceptFragmentOperand @clause l
        rfrag = acceptFragmentOperand @clause r

-- TODO can probably check acceptance here without indirection
-- unless this can be reused for ops
instance (
    AcceptFragmentLiteral clause field lit
  ) => AcceptFragment clause field ('FragLit lit) where
    acceptFragment = acceptFragmentLiteral @clause

------------------------------------------------------------------------------------------------------------------------

type AcceptFragments :: ∀ {ext} . Type -> [Frag ext] -> Type -> (Type -> Type) -> Constraint
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
