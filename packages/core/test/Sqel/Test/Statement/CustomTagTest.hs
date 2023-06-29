module Sqel.Test.Statement.CustomTagTest where

import Exon (exon)
import Hedgehog (TestT, (===))
import Prelude hiding (Mod)

import Sqel.Build.Sql (BuildClause (buildClause), BuildClauseViaDefault (buildClauseViaDefault))
import Sqel.Class.ReifySqel (sqel)
import Sqel.Clauses (from, select)
import Sqel.Data.Dd (Dd)
import Sqel.Data.Migration (Migrate)
import Sqel.Data.Mods.MigrationRename (MigrationRename)
import Sqel.Data.Spine (CompFor, PrimFor)
import Sqel.Data.Sqel (SqelFor)
import Sqel.Data.Sql (Sql)
import Sqel.Data.Statement (statementSql)
import Sqel.Default (CompMeta, PrimMeta)
import Sqel.Dsl (Gen, MigrationTable, Mod, Prim, Prod, Table)
import Sqel.Migration.Class.Syntax ((-->))
import Sqel.Migration.Ddl (Ddl, ToDdl)
import Sqel.Syntax.Fragments (table)
import qualified Sqel.Syntax.Monad as S

data Tag

type instance PrimFor Tag = PrimMeta
type instance CompFor Tag = CompMeta

type SqelTag :: Dd -> Type
type SqelTag = SqelFor Tag

type SqelTagDdl :: Ddl -> Type
type SqelTagDdl = SqelFor Tag

instance BuildClauseViaDefault Tag clause => BuildClause Tag clause where
  buildClause = buildClauseViaDefault @Tag @clause

data Dat0 =
  Dat0 {
    old :: Int
  }
  deriving stock (Eq, Show, Generic)

type Table_Dat0_Ddl = ToDdl (Table "dat" Dat0 Gen)

data Dat =
  Dat {
    a :: Int
  }
  deriving stock (Eq, Show, Generic)

type Table_Dat = Table "dat" Dat Gen

type Table_Dat_Ddl = MigrationTable "dat" Dat (Prod '[Mod (MigrationRename "old") Prim])

table_Dat0 :: SqelTagDdl Table_Dat0_Ddl
table_Dat0 = sqel

table_Dat :: SqelTagDdl Table_Dat_Ddl
table_Dat = sqel

migrations :: Migrate Tag Identity [Table_Dat_Ddl, Table_Dat0_Ddl]
migrations =
  table_Dat0 --> table_Dat

statement :: Sql
statement = statementSql S.do
  t <- table table_Dat
  select t
  from t

target :: Sql
target = [exon|select "a" from "dat"|]

test_customTag :: TestT IO ()
test_customTag =
  target === statement
