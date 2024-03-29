module Sqel.Data.TestTables where

import Sqel.Class.ReifySqel (sqel)
import Sqel.Default (Sqel)
import Sqel.Dsl (Con1, Gen, IntTable, Name, Newtype, Param, Prim, Prod, Query, Sum, Table, Unique)

data Simp =
  Simp {
    name :: Text,
    number :: Int64
  }
  deriving stock (Eq, Show, Generic)

type Table_Simp = Table "simp" Simp Gen

table_Simp :: Sqel Table_Simp
table_Simp = sqel

type UidTable_Simp = IntTable "simp" Simp Gen

uidTable_Simp :: Sqel UidTable_Simp
uidTable_Simp = sqel

data QSimp =
  QSimp {
    name :: Text
  }
  deriving stock (Eq, Show, Generic)

type Query_QSimp = Query QSimp Gen

query_QSimp :: Sqel Query_QSimp
query_QSimp = sqel

data Fur =
  Fur {
    color :: Text,
    density :: Int
  }
  deriving stock (Eq, Show, Generic, Ord)

data Cat =
  Cat {
    num :: Int,
    nam :: Text,
    fur :: Fur
  }
  deriving stock (Eq, Show, Generic, Ord)

data Bird =
  Bird {
    bord :: Int,
    cat :: Text,
    fur :: Fur
  }
  deriving stock (Eq, Show, Generic, Ord)

type Table_Cat = Table "cat" Cat (Prod [Prim, Unique Prim, Gen])
type Table_Bird = Table "bird" Bird (Prod [Prim "num", Prim, Gen])

table_Cat :: Sqel Table_Cat
table_Cat = sqel

table_Bird :: Sqel Table_Bird
table_Bird = sqel

data FurQ =
  FurQ {
    color :: Text
  }
  deriving stock (Eq, Show, Generic)

data Q =
  Q {
    nam :: Maybe Text,
    fur :: FurQ
  }
  deriving stock (Eq, Show, Generic)

type Query_Q = Query Q (Prod [Prim, Gen])

query_Q :: Sqel Query_Q
query_Q = sqel

data NaNu =
  Na { name :: Text }
  |
  Nu Int64
  deriving stock (Eq, Show, Generic)

type Table_NaNu =
  Table "na_nu" NaNu (Sum [Con1 Prim, Con1 Prim])

table_NaNu :: Sqel Table_NaNu
table_NaNu = sqel

type Query_NaNu = Query NaNu (Sum [Gen, Con1 (Name "number" Prim)])

query_NaNu :: Sqel Query_NaNu
query_NaNu = sqel

newtype Nt =
  Nt Int64
  deriving stock (Eq, Show, Generic)

data SI =
  SI1 { si1 :: Nt }
  |
  SI2 { si2 :: Nt }
  deriving stock (Eq, Show, Generic)

type Type_SI = Sum [Con1 Newtype, Con1 Newtype]

data UQ =
  UQ {
    cat :: Text,
    num :: Int64,
    newName :: Text
  }
  deriving stock (Eq, Show, Generic)

type Query_UQ = Query UQ (Prod [Prim, Prim, Param Prim])

query_UQ :: Sqel Query_UQ
query_UQ = sqel
