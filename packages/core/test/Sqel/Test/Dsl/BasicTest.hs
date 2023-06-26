module Sqel.Test.Dsl.BasicTest where

import Data.Type.Equality (type (:~:) (Refl))
import Hedgehog (TestT)
import Prelude hiding (Enum, Mod)

import qualified Sqel.Data.Dd as Kind
import Sqel.Data.Dd (DdK (Dd), Ext0 (Ext0), PrimType (Cond), StructWith (Comp))
import Sqel.Data.Mods (NoMods)
import qualified Sqel.Data.Mods.Array as Mods (Array)
import qualified Sqel.Data.Mods.Nullable as Mods (Nullable)
import qualified Sqel.Data.Mods.Unique as Mods (Unique)
import Sqel.Data.Name (NamePrefix (DefaultPrefix))
import Sqel.Data.Sel (SelAuto, SelName, TSel (TSel))
import Sqel.Dd (SetDdName)
import Sqel.Dsl (Gen, Prim, ProdGen, Reify, Unique)

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

type Table1 = ProdGen Dat1 [Unique Prim, Prim "two" Text, Gen]
type Table2 = Reify Dat2 Gen

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

test_dsl_basic :: TestT IO ()
test_dsl_basic = do
  case Refl :: Table1 :~: Target1 of
    Refl -> unit
  case Refl :: Table2 :~: Target2 of
    Refl -> unit
