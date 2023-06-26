module Sqel.Error.Fragment where

import Fcf (Pure)
import Type.Errors (DelayError, ErrorMessage, IfStuck)

import Sqel.Data.ClauseConfig (ClauseKeywordFor)
import Sqel.Data.Spine (SpineDesc, SpineSort)
import Sqel.Error.Clause (ClauseDesc, GenericClause)
import Sqel.SOP.Error (Quoted, QuotedType, StuckError)

type FragmentMismatch :: Type -> Type -> SpineSort -> k
type family FragmentMismatch tag clause sort

------------------------------------------------------------------------------------------------------------------------

type DefaultDesc :: ErrorMessage -> SpineSort -> ErrorMessage
type family DefaultDesc desc sort where
  DefaultDesc desc sort =
    "Cannot use a " <> SpineDesc sort <> " fragment for a " <> desc <> "."

type DefaultNoDesc :: Type -> SpineSort -> ErrorMessage
type family DefaultNoDesc clause sort where
  DefaultNoDesc clause sort =
    DefaultDesc (GenericClause clause) sort %
    "💥 Note: " <> clause <> " has no " <> Quoted "ClauseDesc"

type FragmentMismatchDefault :: Type -> SpineSort -> k
type family FragmentMismatchDefault clause sort where
  FragmentMismatchDefault clause sort =
    StuckError (ClauseKeywordFor clause)
    (DefaultNoDesc clause sort)
    (DefaultDesc (ClauseDesc clause) sort)

------------------------------------------------------------------------------------------------------------------------

type FullDesc :: Type -> Type -> ErrorMessage -> SpineSort -> ErrorMessage
type family FullDesc tag clause desc sort where
  FullDesc tag clause desc sort =
    "The custom tag " <> QuotedType tag <> " does not accept " <> SpineDesc sort <>
    " fragments for a " <> desc <> "." %
    "You can define an instance to accept it:" %
    "instance AcceptFrag error " <> tag <> " " <> clause <> " " <> sort

type Full :: Type -> Type -> SpineSort -> k
type family Full tag clause sort where
  Full tag clause sort =
    StuckError (ClauseKeywordFor clause)
    (FullDesc tag clause (GenericClause clause) sort)
    (FullDesc tag clause (ClauseDesc clause) sort)

------------------------------------------------------------------------------------------------------------------------

type Inference :: k -> ErrorMessage
type Inference desc =
  "Unable to infer " <> ToErrorMessage desc <> "." %
  "You probably applied a function to too few arguments."

type Confused :: ErrorMessage
type Confused =
  Inference "anything about this fragment"

type OnlySort :: SpineSort -> k
type family OnlySort sort where
  OnlySort sort =
    StuckError sort
    Confused
    (
      "Cannot use a " <> SpineDesc sort <> " fragment here." %
      Inference "tag or clause"
    )

type OnlyDesc :: ErrorMessage -> ErrorMessage
type family OnlyDesc desc where
  OnlyDesc desc =
    "Invalid fragment for a " <> desc <> "." %
    Inference "tag or fragment sort"

type OnlyClause :: Type -> k
type family OnlyClause clause where
  OnlyClause clause =
    StuckError (ClauseKeywordFor clause)
    (OnlyDesc (GenericClause clause))
    (OnlyDesc (ClauseDesc clause))

type DescAndSort :: ErrorMessage -> SpineSort -> ErrorMessage
type family DescAndSort desc sort where
  DescAndSort desc sort =
    "Cannot use a " <> SpineDesc sort <> " fragment for a " <> desc <> "."

------------------------------------------------------------------------------------------------------------------------

type WithClauseCheckSort :: Type -> SpineSort -> k
type family WithClauseCheckSort clause sort where
  WithClauseCheckSort clause sort =
    IfStuck sort
    (OnlyDesc (GenericClause clause))
    (Pure (DescAndSort (GenericClause clause) sort))

type NoTag :: Type -> SpineSort -> k
type family NoTag clause sort where
  NoTag clause sort =
    IfStuck clause
    (OnlySort sort)
    (Pure (WithClauseCheckSort clause sort))

------------------------------------------------------------------------------------------------------------------------

type Custom :: ∀ k . Type -> Type -> SpineSort -> k
type family Custom tag clause sort where
  Custom @k tag clause sort =
    IfStuck (FragmentMismatch tag clause sort :: k)
    (Full tag clause sort)
    (Pure (FragmentMismatch tag clause sort))

type CheckSort :: Type -> Type -> SpineSort -> k
type family CheckSort tag clause sort where
  CheckSort tag clause sort =
    IfStuck sort
    (OnlyClause clause)
    (Pure (Custom tag clause sort))

type CheckClause :: Type -> Type -> SpineSort -> k
type family CheckClause tag clause sort where
  CheckClause tag clause sort =
    IfStuck clause
    (OnlySort sort)
    (Pure (CheckSort tag clause sort))

------------------------------------------------------------------------------------------------------------------------

type RootNoClause :: ErrorMessage
type RootNoClause = ToErrorMessage "This clause does not accept projections."

type RootDesc :: ErrorMessage -> ErrorMessage
type RootDesc desc = "A " <> desc <> " does not accept projections."

type RootClause :: Type -> ErrorMessage -> k
type family RootClause clause desc where
  RootClause clause desc =
    StuckError desc
    (RootDesc (GenericClause clause))
    (RootDesc desc)

type Root :: Type -> ErrorMessage -> k
type family Root clause desc where
  Root clause desc =
    IfStuck clause
    (DelayError RootNoClause)
    (Pure (RootClause clause desc))

------------------------------------------------------------------------------------------------------------------------

type CheckFragmentMismatch :: Type -> Type -> SpineSort -> Bool -> Bool -> k
type family CheckFragmentMismatch tag clause sort acceptSort acceptRoot where
  CheckFragmentMismatch _ _ _ 'True 'True = ()
  CheckFragmentMismatch _ clause _ _ 'False = Root clause (ClauseDesc clause)
  CheckFragmentMismatch tag clause sort 'False _ =
    IfStuck tag
    (NoTag clause sort)
    (Pure (CheckClause tag clause sort))
