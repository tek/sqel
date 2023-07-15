module Sqel.Build.Set where

import Exon (exon)

import Sqel.Build.Columns (spineColumns)
import Sqel.Build.Condition (fieldOpCondition, render)
import Sqel.Build.Values (spineValues)
import Sqel.Data.Def (Def)
import Sqel.Data.Field (CondField (CondField, CondOp), Field (Field))
import Sqel.Data.Spine (Spine)
import Sqel.Data.Sql (Sql)
import Sqel.Sql (joinComma)

-- TODO this allows table and query to be mixed, so the indexes may be wrong, because in @do update set@ we assume that
-- the fragment is a table root that was also the query (for upsert), while @set@ alone is most likely used more
-- flexibly.
-- but even in the first case we might not use the table as a query, so this has to be protected more elaborately.
-- maybe clause configs can map fragment sorts to field types? a sum type that has CondField and Field constructors, and
-- the sort is mapped to the constructor index and injected via NS.

setter :: Sql -> Sql -> Sql
setter name value =
  [exon|#{name} = #{value}|]

tableSetters :: Spine Def -> [Sql]
tableSetters s =
   uncurry setter <$> zip names (foldMap toList (spineValues s))
  where
    names = spineColumns (const True) s

-- TODO op is hardcoded
setters :: CondField Def -> [Sql]
setters (CondField (Field _ s)) = tableSetters s
setters (CondOp _ l r) = maybeToList (render (fieldOpCondition False "=" l r))

setClause :: [CondField Def] -> Sql
setClause fields = joinComma (setters =<< fields)
