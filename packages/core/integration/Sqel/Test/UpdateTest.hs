module Sqel.Test.UpdateTest where

import Hasql.Session (Session)
import Hedgehog (TestT, (===))
import Prelude hiding (join, on)

import Sqel.Clauses (from, set, update, where_)
import Sqel.Data.Statement (Statement)
import Sqel.Data.TestTables (Bird (Bird), Cat (Cat), Fur (Fur), UQ (UQ), query_UQ, table_Bird, table_Cat)
import Sqel.Fragment ((.=))
import Sqel.Migration.Init (initTable)
import Sqel.Statement (runUnprepared)
import qualified Sqel.Statement.Common as Statement
import Sqel.Syntax.Fragments (query)
import qualified Sqel.Syntax.Monad as S
import Sqel.Test.Run (integrationTest, stmt_)

statement :: Statement [Cat, Bird] UQ ()
statement = S.do
  frags <- query query_UQ (table_Cat, table_Bird)
  update frags.cat
  set (frags.cat.nam .= frags.query.newName)
  from frags.bird
  where_ (frags.query, frags.bird.cat .= frags.cat.nam)

fur1 :: Fur
fur1 =
  Fur "red" 10

session :: Session (Set Cat)
session = do
  stmt_ "drop table if exists cat"
  stmt_ "drop table if exists bird"
  stmt_ "drop type if exists sqel_type__fur"
  initTable table_Cat
  initTable table_Bird
  runUnprepared (Cat 1 "cat 1" fur1) insCat
  runUnprepared (Cat 2 "cat 2" fur1) insCat
  runUnprepared (Bird 94 "cat 2" fur1) insBird
  runUnprepared (Bird 174 "cat 1" fur1) insBird
  runUnprepared (Bird 83 "cat 3" fur1) insBird
  runUnprepared (UQ "cat 1" 1 "new cat name") statement
  runUnprepared () (Statement.selectAll table_Cat)
  where
    insCat = Statement.insert table_Cat
    insBird = Statement.insert table_Bird

test_update :: TestT IO ()
test_update =
  integrationTest \ exec -> do
    r <- exec session
    [Cat 1 "new cat name" fur1, Cat 2 "cat 2" fur1] === r
