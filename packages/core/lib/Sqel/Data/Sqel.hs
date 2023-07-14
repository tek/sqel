module Sqel.Data.Sqel where

import GHC.Records (HasField (getField))
import Generics.SOP (K (K), NP (Nil, (:*)), SListI, hcollapse, hmap)

import Sqel.Class.NamedFragment (NoField)
import Sqel.Data.Codec (FullCodec)
import Sqel.Data.Dd (Dd (Dd), Inc (Merge, Nest), SInc (SMerge, SNest), Struct (Comp, Prim))
import Sqel.Data.PgTypeName (PgTableName)
import Sqel.Data.Spine (CompFor, CompSort, PrimFor, Spine (SpineMerge, SpineNest, SpinePrim))
import Sqel.Data.Statement (Statement)
import Sqel.Dd (DdHasName, DdNamesMerged, DdSub, DdType, ExtHasName)
import Sqel.Kind.List (type (++))
import Sqel.SOP.NP (appendNP)

type SqelFor :: ∀ {ext} . Type -> Dd ext -> Type
data SqelFor tag s where

  SqelPrim :: PgTableName -> PrimFor tag -> FullCodec a -> SqelFor tag ('Dd ext a ('Prim pt))

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
      SqelPrim table meta _ -> SpinePrim table meta
      SqelNest meta compSort sub _ -> SpineNest meta compSort (hcollapse (hmap (K . spin) sub))
      SqelMerge meta compSort sub _ -> SpineMerge meta compSort (hcollapse (hmap (K . spin) sub))

-- | Allowing this to be inlined incurs a 5% increase in compile time.
sqelCodec :: SqelFor tag s -> FullCodec (DdType s)
sqelCodec = \case
  SqelPrim _ _ c -> c
  SqelComp _ _ _ _ c -> c
{-# noinline sqelCodec #-}

-- TODO measure performance impact when changing fundep to family
type TryField :: ∀ {ext} . Void -> Bool -> Symbol -> Dd ext -> [Dd ext] -> Dd ext -> Constraint
class TryField error match name s0 ss s | match name s0 ss -> s where
  tryField :: SqelFor tag s0 -> NP (SqelFor tag) ss -> SqelFor tag s

instance TryField error 'True name s0 ss s0 where
  tryField = const

instance (
    FindField error name ss s
  ) => TryField error 'False name s0 ss s where
    tryField _ = findField @error @name

type FindField :: ∀ {ext} . Void -> Symbol -> [Dd ext] -> Dd ext -> Constraint
class FindField error name ss s | name ss -> s where
  findField :: NP (SqelFor tag) ss -> SqelFor tag s

instance {-# overlappable #-} (
    match ~ DdHasName name s0,
    TryField error match name s0 ss s
  ) => FindField error name (s0 : ss) s where
    findField (s0 :* ss) = tryField @error @match @name s0 ss

instance (
    match ~ ExtHasName name ext,
    TryField error match name ('Dd ext a ('Comp tsel sort 'Merge sub)) (sub ++ ss) s
  ) => FindField error name ('Dd ext a ('Comp tsel sort 'Merge sub) : ss) s where
    findField (s@(SqelMerge _ _ sub _) :* ss) = tryField @error @match @name s (appendNP sub ss)

type Projection :: ∀ {ext} . Symbol -> Dd ext -> Dd ext -> Constraint
class Projection name s0 s | name s0 -> s where
  projection :: SqelFor tag s0 -> SqelFor tag s

instance (
    error ~ NoField "field" name (DdNamesMerged sub) sub,
    FindField error name sub s
  ) => Projection name ('Dd ext a ('Comp tsel sort inc sub)) s where
    projection (SqelComp _ _ _ sub _) = findField @error @name sub

instance (
    Projection name s field
  ) => HasField name (SqelFor tag s) (SqelFor tag field) where
    getField = projection @name

sqelSub :: SqelFor tag s -> NP (SqelFor tag) (DdSub s)
sqelSub = \case
  SqelPrim _ _ _ -> Nil
  SqelComp _ _ _ sub _ -> sub

type StatementDd tables query table = Statement tables (DdType query) (DdType table)
