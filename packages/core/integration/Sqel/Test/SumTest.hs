module Sqel.Test.SumTest where

import Hasql.Session (Session)
import Hedgehog (TestT, (===))

import Sqel.Class.ReifySqel (sqel)
import Sqel.Clauses (createType)
import Sqel.Default (Sqel)
import Sqel.Dsl (Gen, Table)
import Sqel.Statement (runUnprepared)
import qualified Sqel.Statement.Common as Statement
import Sqel.Syntax.Fragments (table_)
import qualified Sqel.Syntax.Monad as S
import Sqel.Test.Run (integrationTest, stmt_)

data S =
  S1 { s1 :: Int64 }
  |
  S2 { s2 :: Int64 }
  deriving stock (Eq, Show, Generic, Ord)

type Table_S = Table "s" S Gen

table_S :: Sqel Table_S
table_S = sqel

session :: Session (Set S)
session = do
  stmt_ "drop type if exists sqel_type__s1"
  stmt_ "drop type if exists sqel_type__s2"
  stmt_ "drop table if exists s"
  runUnprepared @() () S.do
    f <- table_ table_S
    createType f.s._S1
  runUnprepared @() () S.do
    f <- table_ table_S
    createType f.s._S2
  runUnprepared () (Statement.createTable table_S)
  runUnprepared (S1 5) (Statement.insert table_S)
  runUnprepared (S2 13) (Statement.insert table_S)
  runUnprepared () (Statement.selectAll table_S)

test_sum :: TestT IO ()
test_sum =
  integrationTest \ exec -> do
    r <- exec session
    [S1 5, S2 13] === r
