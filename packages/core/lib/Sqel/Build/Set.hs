module Sqel.Build.Set where

import Exon (exon)

import Sqel.Build.Columns (spineColumns)
import Sqel.Build.Condition (fieldOpCondition, render)
import Sqel.Build.Values (spineValues)
import Sqel.Data.Def (Def)
import Sqel.Data.Field (CondField (CondField, CondOp), Field (Field))
import Sqel.Data.PgTypeName (PgTableName)
import Sqel.Data.Spine (Spine)
import Sqel.Data.Sql (Sql)
import Sqel.Spine (spineTableName)
import Sqel.Sql (joinComma)

-- TODO this allows table and query to be mixed, so the indexes may be wrong, because in @do update set@ we assume that
-- the fragment is a table root that was also the query (for upsert), while @set@ alone is most likely used more
-- flexibly.
-- but even in the first case we might not use the table as a query, so this has to be protected more elaborately.
-- maybe clause configs can map fragment sorts to field types? a sum type that has CondField and Field constructors, and
-- the sort is mapped to the constructor index and injected via NS.

setter :: Bool -> PgTableName -> Sql -> Sql -> Sql
setter multi table name value =
  [exon|#{pre}#{name} = #{value}|]
  where
    pre | multi = [exon|##{table}.|]
        | otherwise = ""

tableSetters :: Bool -> Spine Def -> [Sql]
tableSetters multi s =
   uncurry (setter multi table) <$> zip names (foldMap toList (spineValues s))
  where
    table = spineTableName s
    names = spineColumns (const True) s

-- TODO op is hardcoded
setters :: Bool -> CondField Def -> [Sql]
setters multi (CondField (Field _ s)) = tableSetters multi s
setters multi (CondOp _ l r) = maybeToList (render (fieldOpCondition multi "=" l r))

setClause :: Bool -> [CondField Def] -> Sql
setClause multi fields = joinComma (setters multi =<< fields)
