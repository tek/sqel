module Sqel.Class.TransformMeta where


import Sqel.Class.DefaultFields (DefaultMeta (defaultCompMeta, defaultPrimMeta))
import qualified Sqel.Data.Spine
import Sqel.Data.Spine (
  CompFor,
  CompSort (CompCon, CompProd, CompSum),
  PrimFor,
  Spine (SpineMerge, SpineNest, SpinePrim),
  TypeSpine (TypeSpine),
  Types (Types),
  spineComp,
  )
import Sqel.Default (Def)

type TransformMeta :: Type -> Type -> Constraint
class TransformMeta tag1 tag2 where
  transformPrimMeta :: PrimFor tag1 -> PrimFor tag2

  transformCompMeta :: CompFor tag1 -> CompFor tag2

  transformedSpineComp :: Bool -> CompFor tag2 -> CompSort tag2 -> [Spine tag2] -> Spine tag2
  transformedSpineComp = spineComp

  transformSpineComp :: Bool -> CompFor tag1 -> CompSort tag1 -> [Spine tag2] -> Spine tag2
  transformSpineComp merge meta compSort sub =
    transformedSpineComp @tag1 merge (transformCompMeta @tag1 @tag2 meta) (transformSortWith (transformPrimMeta @tag1 @tag2) compSort) sub

instance TransformMeta tag tag where
  transformPrimMeta = id
  transformCompMeta = id

instance DefaultMeta tag => TransformMeta tag Def where
  transformPrimMeta = defaultPrimMeta @tag
  transformCompMeta = defaultCompMeta @tag

transformWith ::
  (PrimFor tag1 -> PrimFor tag2) ->
  (Bool -> CompFor tag1 -> CompSort tag1 -> [Spine tag2] -> Spine tag2) ->
  Spine tag1 ->
  Spine tag2
transformWith p c = \case
  SpinePrim meta -> SpinePrim (p meta)
  SpineNest meta compSort sub -> c False meta compSort (transformWith p c <$> sub)
  SpineMerge meta compSort sub -> c True meta compSort (transformWith p c <$> sub)

transformSortWith ::
  (PrimFor tag1 -> PrimFor tag2) ->
  CompSort tag1 ->
  CompSort tag2
transformSortWith p = \case
  CompProd -> CompProd
  CompSum index -> CompSum (p index)
  CompCon -> CompCon

transformMeta ::
  (PrimFor tag1 -> PrimFor tag2) ->
  (CompFor tag1 -> CompFor tag2) ->
  Spine tag1 ->
  Spine tag2
transformMeta p c =
  transformWith p \ merge meta s cols -> spineComp merge (c meta) (transformSortWith p s) cols

type Transform :: Type -> Type -> (Type -> Type) -> Constraint
class Transform tag1 tag2 f where
  transform :: f tag1 -> f tag2

instance (
  TransformMeta tag1 tag2
  ) => Transform tag1 tag2 Spine where
    transform = transformWith (transformPrimMeta @tag1 @tag2) (transformSpineComp @tag1)

instance (
    TransformMeta tag1 tag2
  ) => Transform tag1 tag2 TypeSpine where
    transform TypeSpine {..} =
      TypeSpine {meta = transformCompMeta @tag1 @tag2 meta, sub = transform <$> sub, ..}

instance (
    TransformMeta tag1 tag2
  ) => Transform tag1 tag2 Types where
    transform Types {..} =
      Types {table = transform table, comp = transform <$> comp}
