module Sqel.Test.JoinTest where

import Hasql.Session (Session)
import Hedgehog (TestT, (===))
import Prelude hiding (join, on)

import Sqel.Class.Query (QueryDd)
import Sqel.Class.ReifySqel (ReifySqelFor)
import Sqel.Clauses (createTable, createType, from, insertInto, join, on, select, values, where_)
import Sqel.Data.Dd (Dd)
import Sqel.Data.Def (Def)
import Sqel.Data.Statement (Statement)
import Sqel.Data.TestTables (Bird (Bird), Cat (Cat), Fur (Fur), FurQ (FurQ), Q (Q), Query_Q, Table_Bird, Table_Cat)
import Sqel.Dd (DdType, EmptyQuery)
import Sqel.Fragment ((.=))
import Sqel.Statement (runUnprepared)
import Sqel.Syntax.Fragments (queryK, tableK, tableK_)
import qualified Sqel.Syntax.Monad as S
import Sqel.Test.Run (integrationTest, stmt_)

create ::
  ∀ (table :: Dd) .
  QueryDd Def EmptyQuery '[table] =>
  Statement '[DdType table] () ()
create = S.do
  c <- queryK @EmptyQuery @'[table]
  createTable @Def c.table

ins ::
  ∀ (table :: Dd) .
  ReifySqelFor Def table =>
  Statement '[DdType table] (DdType table) ()
ins = S.do
  t <- tableK @table @Def
  insertInto t
  values t

selJoin :: Statement '[Cat, Bird] Q Int
selJoin = S.do
  c <- queryK @Query_Q @[Table_Cat, Table_Bird] @Def
  select c.bird.num
  from c.cat
  join c.bird
  on (c.cat.nam .= c.bird.cat)
  where_ c.query

session :: Session [Int]
session = do
  stmt_ "drop table if exists cat"
  stmt_ "drop table if exists bird"
  stmt_ "drop type if exists sqel_type__fur"
  runUnprepared @() () S.do
    f <- tableK_ @Table_Cat @Def
    createType f.cat.fur
  runUnprepared () (create @Table_Cat)
  runUnprepared () (create @Table_Bird)
  runUnprepared (Cat 1 "cat 1" (Fur "red" 10)) (ins @Table_Cat)
  runUnprepared (Cat 2 "cat 2" (Fur "red" 15)) (ins @Table_Cat)
  runUnprepared (Bird 94 "cat 2" (Fur "green" 23)) (ins @Table_Bird)
  runUnprepared (Bird 174 "cat 2" (Fur "blue" 23)) (ins @Table_Bird)
  runUnprepared (Bird 83 "cat 3" (Fur "purple" 23)) (ins @Table_Bird)
  runUnprepared (Q (Just "cat 2") (FurQ "red")) selJoin

test_join :: TestT IO ()
test_join =
  integrationTest \ exec -> do
    r <- exec session
    [94, 174] === r
