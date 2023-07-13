module Sqel.Test.Statement.NullableQueryTest where

import Hedgehog (TestT, (===))

import Sqel.Class.ReifySqel (sqel)
import Sqel.Clauses (from, select, where_)
import Sqel.Data.Sql (Sql, sql)
import Sqel.Data.Statement (statementSql)
import Sqel.Default (Def, Sqel)
import Sqel.Dsl (Gen, Newtype, Nullable, OrNull, Prim, Prod, Query, Table)
import Sqel.Syntax.Fragments (query)
import qualified Sqel.Syntax.Monad as S

data Dat =
  Dat {
    num :: Int,
    name :: Text
  }
  deriving stock (Eq, Show, Generic)

type Table_Dat = Table "dat" Dat Gen

dd_Dat :: Sqel Table_Dat
dd_Dat = sqel

newtype Numb =
  Numb { unNumb :: Int }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord)

data Q =
  Q {
    num :: Maybe Numb,
    name :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

type Query_Q = Query Q (Prod [Nullable Newtype, Prim])

type Query_Q_OrNull = Query Q (Prod [OrNull Newtype, OrNull Prim])

target_nullable :: Sql
target_nullable =
  [sql|select "num", "name" from "dat" where "num" = $1 and "name" = $2|]

test_statement_nullableQuery :: TestT IO ()
test_statement_nullableQuery =
  target_nullable === statementSql S.do
    fs <- query @Def (sqel @Query_Q) (sqel @Table_Dat)
    select fs.table
    from fs.table
    where_ fs.query

target_orNull :: Sql
target_orNull =
  [sql|select "num", "name" from "dat" where ($1 is null or "num" = $1) and ($2 is null or "name" = $2)|]

test_statement_orNullQuery :: TestT IO ()
test_statement_orNullQuery =
  target_orNull === statementSql S.do
    fs <- query @Def (sqel @Query_Q_OrNull) (sqel @Table_Dat)
    select fs.table
    from fs.table
    where_ fs.query
