module Sqel.Test.Dsl.BasicTest where

import Data.Type.Equality (type (:~:) (Refl))
import Hedgehog (TestT)
import Prelude hiding (Enum, Mod)

import qualified Sqel.Data.Dd as Kind
import Sqel.Data.Dd (Dd (Dd), Ext (Ext), Ext0 (Ext0), PrimType (Cond), Struct (Comp))
import Sqel.Data.Mods (NoMods)
import qualified Sqel.Data.Mods.Array as Mods (Array)
import qualified Sqel.Data.Mods.Newtype as Mods
import qualified Sqel.Data.Mods.Nullable as Mods (Nullable)
import qualified Sqel.Data.Mods.TableName as Mods
import qualified Sqel.Data.Mods.Unique as Mods (Unique)
import Sqel.Data.Name (NamePrefix (DefaultPrefix))
import Sqel.Data.Sel (Paths (Paths), SelAuto, SelName, TSel (TSel))
import Sqel.Dd (DdTableName, SetDdName)
import Sqel.Dsl (Array, Gen, Name, Newtype, Newtypes, Prim, ProdGen, Reify, Table, Unique)

data Sub =
  Sub {
    three :: Maybe Int,
    four :: [Double]
  }
  deriving stock (Eq, Show, Generic)

type Target_Sub =
  'Dd ('Ext0 SelAuto NoMods) Sub ('Comp ('TSel 'DefaultPrefix "Sub") 'Kind.Prod 'Kind.Nest [
    'Dd ('Ext0 (SelName "three") '[Mods.Nullable 'False]) (Maybe Int) ('Kind.Prim 'Cond),
    'Dd ('Ext0 (SelName "four") '[Mods.Array []]) [Double] ('Kind.Prim 'Cond)
  ])

data Dat1 =
  Dat1 {
    one :: Int,
    two :: Text,
    sub :: Sub
  }
  deriving stock (Eq, Show, Generic)

data Dat2 =
  Dat2 {
    one :: Int,
    two :: Text
  }
  deriving stock (Eq, Show, Generic)

newtype Nt =
  Nt Int64
  deriving stock (Eq, Show, Generic)

newtype Nta =
  Nta [Nt]
  deriving stock (Eq, Show, Generic)

newtype Ntap =
  Ntap [Int64]
  deriving stock (Eq, Show, Generic)

data Dat3 =
  Dat3 {
    ant :: [Nt],
    nta :: Nta,
    ntap :: Ntap
  }
  deriving stock (Eq, Show, Generic)

data Dat4 =
  Dat4 {
    d41 :: Nt,
    d42 :: Nt
  }
  deriving stock (Eq, Show, Generic)

type Table1 = ProdGen Dat1 [Unique Prim, Prim "two" Text, Gen]
type Table2 = Reify Dat2 Gen
type TablePrim = Table "tab" Int64 (Name "col" Prim)
type Table3 = ProdGen Dat3 '[Newtype, Newtype (Array [] Newtype), Newtype]
type Table4 = Reify Dat4 Newtypes

type Target1 =
  'Dd ('Ext0 SelAuto NoMods) Dat1 ('Comp ('TSel 'DefaultPrefix "Dat1") 'Kind.Prod 'Kind.Nest [
    'Dd ('Ext0 (SelName "one") '[Mods.Unique]) Int ('Kind.Prim 'Cond),
    'Dd ('Ext0 (SelName "two") NoMods) Text ('Kind.Prim 'Cond),
    SetDdName "sub" Target_Sub
  ])

type Target2 =
  'Dd ('Ext0 SelAuto NoMods) Dat2 ('Comp ('TSel 'DefaultPrefix "Dat2") 'Kind.Prod 'Kind.Nest [
    'Dd ('Ext0 (SelName "one") NoMods) Int ('Kind.Prim 'Cond),
    'Dd ('Ext0 (SelName "two") NoMods) Text ('Kind.Prim 'Cond)
  ])

type TargetPrim =
  'Dd ('Ext ('Paths "col" '["col"] '["col"]) '[Mods.TableName "tab"]) Int64 ('Kind.Prim 'Cond)

type Target3 =
  'Dd ('Ext0 SelAuto NoMods) Dat3 ('Comp ('TSel 'DefaultPrefix "Dat3") 'Kind.Prod 'Kind.Nest '[
    'Dd ('Ext0 (SelName "ant") [Mods.Array [], Mods.Newtype Nt Int64]) [Nt] ('Kind.Prim 'Cond),
    'Dd ('Ext0 (SelName "nta") [Mods.Newtype Nta [Nt], Mods.Array [], Mods.Newtype Nt Int64]) Nta ('Kind.Prim 'Cond),
    'Dd ('Ext0 (SelName "ntap") [Mods.Newtype Ntap [Int64], Mods.Array []]) Ntap ('Kind.Prim 'Cond)
  ])

type Target4 =
  'Dd ('Ext0 SelAuto NoMods) Dat4 ('Comp ('TSel 'DefaultPrefix "Dat4") 'Kind.Prod 'Kind.Nest '[
    'Dd ('Ext0 (SelName "d41") '[Mods.Newtype Nt Int64]) Nt ('Kind.Prim 'Cond),
    'Dd ('Ext0 (SelName "d42") '[Mods.Newtype Nt Int64]) Nt ('Kind.Prim 'Cond)
  ])

test_dsl_basic :: TestT IO ()
test_dsl_basic = do
  case Refl :: Table1 :~: Target1 of
    Refl -> unit
  case Refl :: Table2 :~: Target2 of
    Refl -> unit
  case Refl :: TablePrim :~: TargetPrim of
    Refl -> unit
  case Refl :: "tab" :~: DdTableName TargetPrim of
    Refl -> unit
  case Refl :: Table3 :~: Target3 of
    Refl -> unit
  case Refl :: Table4 :~: Target4 of
    Refl -> unit
