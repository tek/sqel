module Sqel.Test.Statement.MinimalTest where

import Exon (exon)
import Hedgehog (TestT, (===))

import Sqel.Class.ReifySqel (sqel)
import Sqel.Clauses (from, select)
import Sqel.Data.Sql (Sql)
import Sqel.Default (Sqel)
import Sqel.Dsl (Gen, Table)
import Sqel.Syntax.Fragments (table)
import qualified Sqel.Syntax.Monad as S

data Dat =
  Dat {
    a :: Int
  }
  deriving stock (Eq, Show, Generic)

type Table_Dat = Table "dat" Dat Gen

table_Dat :: Sqel Table_Dat
table_Dat = sqel

statement :: Sql
statement = S.do
  t <- table table_Dat
  select t
  from t

target :: Sql
target = [exon|select "a" from "dat"|]

test_minimal :: TestT IO ()
test_minimal =
  target === statement
