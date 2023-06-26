module Sqel.Data.Sqel where

import GHC.Records (HasField (getField))
import Generics.SOP (K (K), NP (Nil), SListI, hcollapse, hmap)

import Sqel.Class.DdField (DdKField (ddKField), DdKFieldT)
import Sqel.Data.Codec (FullCodec)
import Sqel.Data.Dd (DdK (Dd), Inc (Merge, Nest), SInc (SMerge, SNest), StructWith (Comp, Prim))
import Sqel.Data.Spine (CompFor, CompSort, PrimFor, Spine (SpineMerge, SpineNest, SpinePrim))
import Sqel.Dd (DdSub, DdType)
import Sqel.SOP.Error (Quoted)

type SqelFor :: ∀ {ext} . Type -> DdK ext -> Type
data SqelFor tag s where

  SqelPrim :: PrimFor tag -> FullCodec a -> SqelFor tag ('Dd ext a ('Prim pt))

  SqelComp ::
    SListI sub =>
    SInc inc ->
    CompFor tag ->
    CompSort tag ->
    NP (SqelFor tag) sub ->
    FullCodec a ->
    SqelFor tag ('Dd ext a ('Comp tsel sort inc sub))

deriving stock instance (
    Eq (PrimFor tag),
    Eq (FullCodec a)
  ) => Eq (SqelFor tag ('Dd ext a ('Prim pt)))

deriving stock instance (
    Eq (PrimFor tag),
    Eq (CompFor tag),
    Eq (NP (SqelFor tag) sub),
    Eq (FullCodec a)
  ) => Eq (SqelFor tag ('Dd ext a ('Comp tsel sort inc sub)))

deriving stock instance (
    Show (PrimFor tag),
    Show (FullCodec a)
  ) => Show (SqelFor tag ('Dd ext a ('Prim pt)))

deriving stock instance (
    Show (PrimFor tag),
    Show (CompFor tag),
    Show (NP (SqelFor tag) sub),
    Show (FullCodec a)
  ) => Show (SqelFor tag ('Dd ext a ('Comp tsel sort inc sub)))

pattern SqelNest ::
  () =>
  (s ~ 'Dd ext a ('Comp tsel sort 'Nest sub), SListI sub) =>
  CompFor tag ->
  CompSort tag ->
  NP (SqelFor tag) sub ->
  FullCodec a ->
  SqelFor tag s
pattern SqelNest meta sort sub codec = SqelComp SNest meta sort sub codec

pattern SqelMerge ::
  () =>
  (s ~ 'Dd ext a ('Comp tsel sort 'Merge sub), SListI sub) =>
  CompFor tag ->
  CompSort tag ->
  NP (SqelFor tag) sub ->
  FullCodec a ->
  SqelFor tag s
pattern SqelMerge meta sort sub codec = SqelComp SMerge meta sort sub codec

{-# complete SqelPrim, SqelNest, SqelMerge #-}

sqelSpine ::
  ∀ tag s .
  SqelFor tag s ->
  Spine tag
sqelSpine =
  spin
  where
    spin :: ∀ s' . SqelFor tag s' -> Spine tag
    spin = \case
      SqelPrim meta _ -> SpinePrim meta
      SqelNest meta compSort sub _ -> SpineNest meta compSort (hcollapse (hmap (K . spin) sub))
      SqelMerge meta compSort sub _ -> SpineMerge meta compSort (hcollapse (hmap (K . spin) sub))

sqelCodec :: SqelFor tag s -> FullCodec (DdType s)
sqelCodec = \case
  SqelPrim _ c -> c
  SqelComp _ _ _ _ c -> c

type Projected :: ∀ {ext} . Symbol -> DdK ext -> DdK ext
type family Projected name s where
  Projected name ('Dd _ _ ('Comp _ _ _ sub)) = DdKFieldT name sub
  Projected name ('Dd _ _ ('Prim _)) =
    TypeError ("Cannot project field named " <> Quoted name <> " from primitive Dd")

type Project :: ∀ {ext} . Symbol -> DdK ext -> Constraint
class Project name s where
  project :: SqelFor tag s -> SqelFor tag (Projected name s)

instance DdKField name sub => Project name ('Dd ext a ('Comp tsel sort inc sub)) where
    project (SqelComp _ _ _ sub _) = ddKField @name sub

instance (
    Project name s,
    field ~ Projected name s
  ) => HasField name (SqelFor tag s) (SqelFor tag field) where
    getField = project @name

sqelSub :: SqelFor tag s -> NP (SqelFor tag) (DdSub s)
sqelSub = \case
  SqelPrim _ _ -> Nil
  SqelComp _ _ _ sub _ -> sub
