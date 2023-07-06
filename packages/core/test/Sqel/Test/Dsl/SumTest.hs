module Sqel.Test.Dsl.SumTest where

import Data.Type.Equality (type (:~:) (Refl))
import Hedgehog (TestT)

import qualified Sqel.Data.Dd as Kind
import Sqel.Data.Dd (ConCol, DdK (Dd), Ext0 (Ext0), PrimType (Cond), StructWith (Comp))
import Sqel.Data.Mods (NoMods)
import Sqel.Data.Name (NamePrefix (DefaultPrefix))
import Sqel.Data.Sel (SelAuto, SelName, SelSkip, TSel (TSel), TSelNamed)
import Sqel.Dsl (Con1, Gen, Prim, Reify, Sum)
import Sqel.Data.TestTables (NaNu)

type Table_NaNu_1 =
  Reify NaNu (Sum [Con1 Prim, Con1 Prim])

type Table_NaNu_2 =
  Reify NaNu Gen

type Table_NaNu_3 =
  Reify NaNu (Sum [Gen, Gen])

type Target_NaNu =
  'Dd ('Ext0 SelAuto NoMods) NaNu ('Comp ('TSel 'DefaultPrefix "NaNu") ('Kind.Sum 'DefaultPrefix) 'Kind.Nest [
    'Dd ('Ext0 (SelSkip "Na") NoMods) (ConCol '[Text]) ('Comp (TSelNamed "Na") 'Kind.Con 'Kind.Merge '[
      'Dd ('Ext0 (SelName "name") '[]) Text ('Kind.Prim 'Cond)
      ]),
    'Dd ('Ext0 (SelSkip "Nu") NoMods) (ConCol '[Int64]) ('Comp (TSelNamed "Nu") 'Kind.Con 'Kind.Merge '[
      'Dd ('Ext0 (SelName "Nu") '[]) Int64 ('Kind.Prim 'Cond)
      ])
  ])

test_dsl_con1 :: TestT IO ()
test_dsl_con1 = do
  case Refl :: Table_NaNu_1 :~: Target_NaNu of
    Refl -> unit
  case Refl :: Table_NaNu_2 :~: Target_NaNu of
    Refl -> unit
  case Refl :: Table_NaNu_3 :~: Target_NaNu of
    Refl -> unit
