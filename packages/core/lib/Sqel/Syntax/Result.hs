module Sqel.Syntax.Result where

import Sqel.Build.Statement (BuildStatement (buildStatement))
import Sqel.Data.Clause (Clauses)
import Sqel.Data.Fragment (Frag (Frag), Frag0 (Frag0), Fragment (Fragment))
import Sqel.Data.Fragments (Fragments, maybeQuery)
import Sqel.Data.Spine (SpineSort (SpineTable))
import Sqel.Data.Statement (Statement)
import Sqel.Dd (DdType, MaybeDdType)
import Sqel.Kind.Maybe (MaybeD (JustD))

---- TODO would be nice if both @table@ and @table_@ could return Fragment.
---- Right now @table_@ uses Fragments to signal that there's no query, I guess because using @'Just table@ for the query
---- with @table@ would cause overlap.

type DoResult :: Type -> Type -> [Type] -> Maybe [Type] -> Type -> Type -> Type -> Constraint
class DoResult frags tag cs results a q result | frags -> q where
  doResult :: frags -> Clauses tag cs results a -> Statement q result

instance (
   BuildStatement tag ('Just table) cs results result,
   q ~ DdType table
  ) => DoResult (Fragment ('Frag ('Frag0 tag 'SpineTable table 'True comp))) tag cs results a q result where
    doResult (Fragment frag) cs = buildStatement @tag @('Just table) @cs False cs (JustD frag)

instance {-# overlappable #-} (
    BuildStatement tag query cs results result,
    q ~ MaybeDdType query
  ) => DoResult (Fragments tag query tables projs) tag cs results a q result where
    doResult frags cs = buildStatement @tag @query @cs frags.multi cs (maybeQuery frags)
