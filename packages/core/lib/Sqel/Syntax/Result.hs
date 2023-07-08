module Sqel.Syntax.Result where

import Sqel.Build.Statement (BuildStatement (buildStatement))
import Sqel.Data.Clause (Clauses)
import Sqel.Data.Fragment (Frag (Frag), Frag0 (Frag0), Fragment (Fragment))
import Sqel.Data.Fragments (Fragments, maybeQuery)
import Sqel.Data.Spine (SpineSort (SpineTable))
import Sqel.Data.Statement (Statement)
import Sqel.Dd (DdType, DdTypes, MaybeDdType)
import Sqel.Kind.Maybe (MaybeD (JustD))

---- TODO would be nice if both @table@ and @table_@ could return Fragment.
---- Right now @table_@ uses Fragments to signal that there's no query, I guess because using @'Just table@ for the query
---- with @table@ would cause overlap.

type DoResult :: Type -> Type -> [Type] -> Maybe [Type] -> Type -> [Type] -> Type -> Type -> Constraint
class DoResult frags tag cs results a tables q result | frags -> tables q where
  doResult :: frags -> Clauses tag cs results a -> Statement tables q result

instance (
    t ~ DdType table,
    BuildStatement tag '[t] ('Just table) cs results result,
    q ~ DdType table
  ) => DoResult (Fragment ('Frag ('Frag0 tag 'SpineTable table 'True comp))) tag cs results a '[t] q result where
    doResult (Fragment frag) cs = buildStatement @tag @'[t] @('Just table) @cs False cs (JustD frag)

instance {-# overlappable #-} (
    ts ~ DdTypes tables,
    BuildStatement tag ts query cs results result,
    q ~ MaybeDdType query
  ) => DoResult (Fragments tag query tables projs) tag cs results a ts q result where
    doResult frags cs = buildStatement @tag @ts @query @cs frags.multi cs (maybeQuery frags)
