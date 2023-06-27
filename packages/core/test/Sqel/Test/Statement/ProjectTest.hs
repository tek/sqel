module Sqel.Test.Statement.ProjectTest where

import Exon (exon)
import Hedgehog (TestT, (===))

import Sqel.Class.Check (Check)
import Sqel.Class.NamedFragment (NamedProjection (ProjectionNamed))
import Sqel.Clauses (from, select, where_)
import Sqel.Data.Sql (Sql)
import Sqel.Data.Statement (Statement)
import Sqel.Dd (DdType)
import Sqel.Default (Sqel)
import Sqel.Syntax.Fragments (project, query1)
import qualified Sqel.Syntax.Monad as S
import Sqel.Test.Statement.Common (query_Q, table_Cat)

statement1 ::
  ∀ query table proj .
  Check '[table] query =>
  Check '[table] proj =>
  NamedProjection "fur" '[proj] =>
  Sqel query ->
  Sqel table ->
  Sqel proj ->
  Sql
statement1 query table proj = S.do
  frags <- project proj (query1 query table)
  select frags.projections.fur
  from frags.table
  where_ frags.query

-- TODO when using the wrong result type, the error doesn't show @ProjectionNamed@, but @HFindT@.
-- Would be good to have a custom error here, unclear whether it's feasible.
statement2 ::
  ∀ query table proj .
  Check '[table] query =>
  Check '[table] proj =>
  NamedProjection "fur" '[proj] =>
  Sqel query ->
  Sqel table ->
  Sqel proj ->
  Statement (DdType query) (DdType (ProjectionNamed "fur" '[proj]))
statement2 query table proj = S.do
  frags <- project proj (query1 query table)
  select frags.projections.fur
  from frags.table
  where_ frags.query

target1 :: Sql
target1 = [exon|select ("fur")."color", ("fur")."density" from "cat" where "nam" = $1 and ("fur")."color" = $2|]

test_project :: TestT IO ()
test_project = do
  target1 === statement1 query_Q table_Cat table_Cat.fur