module Sqel.Test.NestedSumTest where

import Hasql.Session (Session)
import Hedgehog (TestT, (===))

import Sqel.Class.ReifySqel (sqel)
import Sqel.Default (Sqel)
import Sqel.Dsl (Con1, Gen, Prim, Sum, Table)
import Sqel.Migration.Init (initTable)
import Sqel.Statement (runUnprepared)
import qualified Sqel.Statement.Common as Statement
import Sqel.Test.Run (integrationTest, stmt_)

data N =
  N1 { n11 :: Int64, n12 :: Int64 }
  |
  N2 { n21 :: Int64, n22 :: Int64 }
  deriving stock (Eq, Show, Generic, Ord)

data S =
  S1 { s1 :: N }
  |
  S2 { s2 :: Int64 }
  deriving stock (Eq, Show, Generic, Ord)

type Table_S = Table "s" S (Sum [Con1 Gen, Con1 Prim])

table_S :: Sqel Table_S
table_S = sqel

session :: Session (Set S)
session = do
  stmt_ "drop table if exists s"
  stmt_ "drop type if exists sqel_type__s1"
  stmt_ "drop type if exists sqel_type__s2"
  stmt_ "drop type if exists sqel_type__n"
  stmt_ "drop type if exists sqel_type__n1"
  stmt_ "drop type if exists sqel_type__n2"
  initTable table_S
  runUnprepared (S1 (N1 5 6)) ins
  runUnprepared (S1 (N2 11 12)) ins
  runUnprepared (S2 13) ins
  runUnprepared () (Statement.selectAll table_S)
  where
    ins = Statement.insert table_S

test_nestedSum :: TestT IO ()
test_nestedSum =
  integrationTest \ exec -> do
    r <- exec session
    [S1 (N1 5 6), S1 (N2 11 12), S2 13] === r
