module Sqel.Test.Statement.BuildTest where

import Exon (exon)
import Exon.SkipWs (intron)
import Generics.SOP (NP (Nil, (:*)))
import Hedgehog (TestT, (===))
import Prelude hiding (Mod, join, on)

import Sqel.Build (buildAs, buildKAs, buildKTuple, buildTuple)
import Sqel.Build.Sql (BuildClause, BuildClauses, buildSqlDd)
import Sqel.Class.Check (Check, Checked)
import Sqel.Class.Query (FragmentsDd)
import Sqel.Clauses (createTable, from, join, on, select, where_)
import Sqel.Data.Clause ((+>))
import Sqel.Data.Dd (Dd, type (:>) ((:>)))
import Sqel.Data.Sqel (Project, Projected, SqelFor, StatementDd)
import Sqel.Data.Sql (Sql)
import Sqel.Data.Statement (Statement)
import Sqel.Data.TestTables (Bird, Cat, Fur, Q, Query_Q, Table_Bird, Table_Cat)
import Sqel.Dd (DdType)
import Sqel.Default (CreateTable, Def, From, Select, Sqel, Where)
import Sqel.Fragment ((.=))
import Sqel.Kind.Maybe (MaybeD (JustD, NothingD))
import Sqel.Syntax.Fragments (project, query1)
import qualified Sqel.Syntax.Monad as Sqel

stmt1 ::
  ∀ {ext} (query :: Dd ext) .
  FragmentsDd Def ('Just query) '[Table_Cat] =>
  Project "fur" query =>
  Sql
stmt1 =
  buildSqlDd @('Just query) @'[Table_Cat] @Def \ c ->
    select c.cat.fur +> from c.cat +> where_ c.query.fur

type PF res = Projected "fur" res

-- TODO the error message trying to be achieved:
-- Could not deduce 'Check <tables> res' for the polymorphic type 'res'.
-- The type for '<tables>' is elided here since it is usually an extremely verbose Dd kind.
-- The class 'Check' performs query validation.
-- If you want to see the full type, call 'statementWith @(RawErrors tag)' (insert 'tag' from what's present).
--
-- However, since we've now achieved automatic result type extraction, this can be specialized to the query type.
stmt2 :: Statement '[Cat] Q Fur
stmt2 =
  buildKAs @('Just Query_Q) @'[Table_Cat] \ c ->
    select c.cat.fur +> from c.cat +> where_ c.query.fur

stmt3 ::
  ∀ q d .
  Check '[d] q =>
  Sqel q ->
  Sqel d ->
  StatementDd '[DdType d] q d
stmt3 q d =
  buildAs @(DdType d) (JustD q) (d :* Nil) \ frags ->
    select frags.table +> from frags.table +> where_ frags.query

stmt4 :: Sql
stmt4 =
  buildSqlDd @('Just Query_Q) @'[Table_Cat] @Def \ c ->
    select c.cat +> from c.cat +> where_ c.query.fur

-- -- TODO QuerySqel should be mentioned in error message if stuck
-- TODO apparently this is too polymorphic since the decoupling of the ext kinds of query/table
-- stmt5 ::
--   ∀ {extt} res (tables :: [Dd extt]) .
--   ReifySqels Def tables =>
--   NamedTable "cat" tables =>
--   NamedTable "bird" tables =>
--   Project "cat" (TableNamed "bird" tables) =>
--   Project "nam" (TableNamed "cat" tables) =>
--   QuerySqel Def Query_Q tables =>
--   ResultDecoder '[DdType (TableNamed "cat" tables)] res =>
--   Statement Q res
-- stmt5 =
--   buildKAs @('Just Query_Q) @tables @res \ c ->
--     select c.cat +>
--     from c.cat +>
--     join c.bird +>
--     on (c.cat.nam .= c.bird.cat) +>
--     where_ c.query.fur

stmt6 :: Statement '[Cat, Bird] Q Cat
stmt6 =
  buildKTuple @('Just Query_Q) @[Table_Cat, Table_Bird] \ c ->
      select c.cat +>
      from c.cat +>
      join c.bird +>
      on (c.cat.nam .= c.bird.cat) +>
      where_ c.query.fur

stmt7 :: Sql
stmt7 =
  buildSqlDd @('Just Query_Q) @[Table_Cat, Table_Bird] @Def \ c ->
    select c.cat +>
    from c.cat +>
    join c.bird +>
    on (c.cat.nam .= c.bird.cat) +>
    where_ c.query.fur

stmt8 :: Statement '[Cat] Q (Text, Int)
stmt8 =
  buildKTuple @('Just Query_Q) @'[Table_Cat] \ c ->
    select (c.cat.fur.color, c.cat.fur.density) +> from c.cat +> where_ c.query.fur

data TextInt =
  TextInt Text Int
  deriving stock (Generic)

stmt9 :: Statement '[Cat] Q TextInt
stmt9 =
  buildKAs @('Just Query_Q) @'[Table_Cat] \ c ->
    select (c.cat.fur.color :> c.cat.fur.density) +> from c.cat +> where_ c.query.fur

stmt10 :: Sql
stmt10 =
  buildSqlDd @('Just Query_Q) @'[Table_Cat] @Def \ c ->
    select (c.cat.fur.color :> c.cat.fur.density) +>
    from c.cat +>
    where_ c.query.fur

stmt11 :: Statement '[Cat] Q TextInt
stmt11 =
  buildKAs @('Just Query_Q) @'[Table_Cat] \ c ->
    select (c.cat.fur.color, c.cat.fur.density) +>
    from c.cat +>
    where_ c.query.fur

stmt12 ::
  BuildClause tag CreateTable =>
  SqelFor tag Table_Cat ->
  Statement '[Cat] () ()
stmt12 table =
  buildTuple NothingD (table :* Nil) \ c ->
    createTable c.table

selectQuery ::
  ∀ {extq} {extt} (query :: Dd extq) (proj :: Dd extt) (table :: Dd extt) tag .
  BuildClauses tag [Select, From, Where] =>
  Checked '[table] tag proj =>
  Checked '[table] tag query =>
  SqelFor tag query ->
  SqelFor tag proj ->
  SqelFor tag table ->
  Statement '[DdType table] (DdType query) (DdType proj)
selectQuery query proj table = Sqel.do
  frags <- project proj (query1 query table)
  select frags.projection
  from frags.table
  where_ frags.query

target1 :: Sql
target1 = [exon|select ("fur")."color", ("fur")."density" from "cat" where ("fur")."color" = $2|]

target4 :: Sql
target4 = [exon|select "num", "nam", ("fur")."color", ("fur")."density" from "cat" where ("fur")."color" = $2|]

target7 :: Sql
target7 = [intron|
  select "cat"."num", "cat"."nam", ("cat"."fur")."color", ("cat"."fur")."density"
  from "cat" cat join "bird" bird
  on "cat"."nam" = "bird"."cat"
  where ("cat"."fur")."color" = $2
  |]

test_build :: TestT IO ()
test_build = do
  target1 === stmt1 @Query_Q
  target4 === stmt4
  target7 === stmt7
  -- where
  --   _ = stmt5 @Cat @'[Table_Cat, Table_Bird]
