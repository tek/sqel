module Sqel.Test.EnumTest where

import Hedgehog (TestT, (===))
import Prelude hiding (Enum)

import Sqel.Class.ReifyCodec (reifyCodec)
import Sqel.Class.ReifySqel (sqel)
import Sqel.Clauses (createTable, from, select)
import Sqel.Data.Codec (Decoder)
import Sqel.Data.Migration (Migrate)
import Sqel.Data.Sql (Sql, sql)
import Sqel.Data.Statement (Statement, statementSql)
import Sqel.Default (Def, Sqel)
import Sqel.Dsl (Enum, Prim, Prod, Table)
import Sqel.Kind.HasFieldK (type (.))
import Sqel.Migration.Class.Syntax ((-->))
import Sqel.Migration.Data.TransformStep (TransformStep)
import Sqel.Migration.Transform (transform)
import Sqel.Syntax.Fragments (tableK, tableK_)
import qualified Sqel.Syntax.Monad as S

data Old =
  Old {
    old :: Int
  }
  deriving stock (Eq, Show, Generic)

type Table_Old = Table "e" Old (Prod '[Prim])

table_Old :: Sqel Table_Old
table_Old = sqel

data En =
  En1 | En2
  deriving stock (Eq, Show, Generic, Ord)

data E =
  E {
    f1 :: Int,
    f2 :: En,
    f3 :: Set En
  }
  deriving stock (Generic)

type Table_E =
  Table "e" E (Prod [Prim, Enum, Enum])

table_E :: Sqel Table_E
table_E = sqel

sql_e :: Sql
sql_e = statementSql @_ @_ @() S.do
  t <- tableK @Table_E @Def
  createTable t

statement_e :: Statement '[E] () E
statement_e = S.do
  frags <- tableK_ @Table_E @Def
  select frags.table
  from frags.table

step :: TransformStep Def Identity Old Table_E
step =
  transform trans table_E
  where
    trans :: [Old] -> Identity [E]
    trans old = pure (E 5 En1 [En2, En1] <$ old)

decEn :: Decoder En
decEn = reifyCodec @Decoder @(Table_E . "f2")

dec :: Decoder E
dec = reifyCodec @Decoder @Table_E

mig :: Migrate Def Identity [Table_E, Table_Old]
mig =
  table_Old --> step

test_enum :: TestT IO ()
test_enum =
  [sql|create table "e" ("f1" bigint not null, "f2" text not null, "f3" text[] not null)|] === sql_e
