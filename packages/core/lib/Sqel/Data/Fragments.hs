module Sqel.Data.Fragments where

import GHC.Records (HasField (getField))
import Generics.SOP (NP (Nil, (:*)))

import Sqel.Class.NamedFragment (NamedProjection (namedProjection), NamedTable (namedTable))
import Sqel.Data.Dd (DdK)
import Sqel.Data.Fragment (Frag (Frag), Frag0 (Frag0), Fragment (Fragment))
import Sqel.Data.Spine (SpineSort (SpineProj, SpineQuery, SpineTable))
import Sqel.Data.Sqel (SqelFor)
import Sqel.Dd (IsComp)
import Sqel.Kind.Maybe (MaybeD (JustD, NothingD))

type QueryFragment :: Type -> Maybe (DdK ext) -> Type
data QueryFragment tag s where
  QueryFragment :: Fragment ('Frag ('Frag0 tag 'SpineQuery s 'True (IsComp s))) -> QueryFragment tag ('Just s)
  NoQueryFragment :: QueryFragment tag 'Nothing

type TableFragment :: Type -> DdK ext -> Type
newtype TableFragment tag s =
  TableFragment (Fragment ('Frag ('Frag0 tag 'SpineTable s 'True (IsComp s))))

type TableFragments :: Type -> [DdK ext] -> Type
newtype TableFragments tag tables =
  TableFragments (NP (TableFragment tag) tables)

instance (
    NamedTable name tables s,
    comp ~ IsComp s
  ) => HasField name (TableFragments tag (tables :: [DdK ext])) (Fragment ('Frag ('Frag0 tag 'SpineTable (s :: DdK ext) 'True comp))) where
    getField (TableFragments ss) =
      coerce (namedTable @name ss)

type ProjectionFragment :: Type -> DdK ext -> Type
newtype ProjectionFragment tag s =
  ProjectionFragment (Fragment ('Frag ('Frag0 tag 'SpineProj s 'False (IsComp s))))

type ProjectionFragments :: Type -> [DdK ext] -> Type
newtype ProjectionFragments tag tables =
  ProjectionFragments (NP (ProjectionFragment tag) tables)

instance (
    NamedProjection name proj s,
    comp ~ IsComp s
  ) => HasField name (ProjectionFragments tag (proj :: [DdK ext])) (Fragment ('Frag ('Frag0 tag 'SpineProj (s :: DdK ext) 'False comp))) where
    getField (ProjectionFragments ss) =
      coerce (namedProjection @name ss)

type Fragments ::
  ∀ {extq} {extt} .
  Type ->
  Maybe (DdK extq) ->
  [DdK extt] ->
  [DdK extt] ->
  Type
data Fragments tag query tables proj where
  Fragments ::
    QueryFragment tag query ->
    TableFragments tag tables ->
    ProjectionFragments tag proj ->
    Bool ->
    Fragments tag query tables proj

data FragmentsField =
  DdField Symbol
  |
  FragmentsTable Symbol
  |
  FragmentsSingleTable
  |
  FragmentsSingleProj

type SelectFragmentsField :: Symbol -> FragmentsField
type family SelectFragmentsField name where
  SelectFragmentsField "query" = 'DdField "query"
  SelectFragmentsField "tables" = 'DdField "tables"
  SelectFragmentsField "projections" = 'DdField "projections"
  SelectFragmentsField "multi" = 'DdField "multi"
  SelectFragmentsField "table" = 'FragmentsSingleTable
  SelectFragmentsField "projection" = 'FragmentsSingleProj
  SelectFragmentsField name = 'FragmentsTable name

type GetFragmentsField ::
  ∀ {extq} {extt} .
  FragmentsField ->
  Type ->
  Maybe (DdK extq) ->
  [DdK extt] ->
  [DdK extt] ->
  Type ->
  Constraint
class GetFragmentsField field tag query tables proj r | field tag query tables proj -> r where
  getFragmentsField :: Fragments tag query tables proj -> r

-- TODO add error instance for 'Nothing
instance (
    comp ~ IsComp query
  ) => GetFragmentsField ('DdField "query") tag ('Just query) tables proj (Fragment ('Frag ('Frag0 tag 'SpineQuery query 'True comp))) where
    getFragmentsField (Fragments (QueryFragment query) _ _ _) = query

instance GetFragmentsField ('DdField "tables") tag query tables proj (TableFragments tag tables) where
    getFragmentsField (Fragments _ tables _ _) = tables

instance GetFragmentsField ('DdField "projections") tag query tables proj (ProjectionFragments tag proj) where
    getFragmentsField (Fragments _ _ proj _) = proj

instance GetFragmentsField ('DdField "multi") tag query tables proj Bool where
    getFragmentsField (Fragments _ _ _ multi) = multi

instance (
    HasField name (TableFragments tag tables) r
  ) => GetFragmentsField ('FragmentsTable name) tag query tables proj r where
    getFragmentsField (Fragments _ tables _ _) = getField @name tables

instance (
    comp ~ IsComp table
  ) => GetFragmentsField 'FragmentsSingleTable tag query '[table] proj (Fragment ('Frag ('Frag0 tag 'SpineTable table 'True comp))) where
    getFragmentsField (Fragments _ (TableFragments (TableFragment table :* Nil)) _ _) = table

instance (
    comp ~ IsComp proj
  ) => GetFragmentsField 'FragmentsSingleProj tag query tables '[proj] (Fragment ('Frag ('Frag0 tag 'SpineProj proj 'False comp))) where
    getFragmentsField (Fragments _ _ (ProjectionFragments (ProjectionFragment proj :* Nil)) _) = proj

instance (
    field ~ SelectFragmentsField name,
    GetFragmentsField field tag query tables proj r
  ) => HasField name (Fragments tag query tables proj) r where
    getField = getFragmentsField @field

maybeQuery :: Fragments tag query tables proj -> MaybeD (SqelFor tag) query
maybeQuery (Fragments query _ _ _) =
  case query of
    QueryFragment (Fragment s) -> JustD s
    NoQueryFragment -> NothingD
