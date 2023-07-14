module Sqel.Class.Check where

import Generics.SOP (All, hcmap)

import Sqel.Data.Dd (Dd (Dd), PrimType (Cond, NoCond), Struct (Comp, Prim))
import Sqel.Data.Fragment (Frag (Frag), Frag0 (Frag0), Fragment (Fragment))
import Sqel.Data.PgTypeName (pgTableName)
import Sqel.Data.Sel (Paths)
import Sqel.Data.Sqel (SqelFor (SqelPrim), pattern SqelMerge, pattern SqelNest)
import Sqel.Dd (DdTableName, ExtPath, IsComp)
import Sqel.Kind.CheckPrim (CheckPrim)
import Sqel.Kind.TableInfo (MkTableInfo, TableInfo (AvailTables, IsTable))
import Sqel.SOP.Constraint (symbolText)

type PrimTable :: ∀ {ext} . TableInfo ext -> Paths -> Type -> Constraint
class PrimTable table path a where
  primTable :: Text

instance KnownSymbol table => PrimTable ('IsTable table) path a where
  primTable = symbolText @table

instance (
    table ~ CheckPrim tables a path,
    KnownSymbol table
  ) => PrimTable ('AvailTables tables) path a where
    primTable = symbolText @table

------------------------------------------------------------------------------------------------------------------------

type Node :: ∀ {ext} . TableInfo ext -> Dd ext -> Constraint
class Node table s where
  node :: SqelFor tag s -> SqelFor tag s

instance (
    path ~ ExtPath ext,
    PrimTable table path a
  ) => Node table ('Dd ext a ('Prim 'Cond)) where
    node (SqelPrim _ p codec) =
      SqelPrim (pgTableName (primTable @table @path @a)) p codec

instance Node table ('Dd ext a ('Prim 'NoCond)) where
  node = id

instance (
    All (Node table) sub
  ) => Node table ('Dd ext a ('Comp tsel c i sub)) where
    node = \case
      SqelNest c compSort sub codec ->
        SqelNest c compSort (hcmap (Proxy @(Node table)) (node @table) sub) codec
      SqelMerge c compSort sub codec ->
        SqelMerge c compSort (hcmap (Proxy @(Node table)) (node @table) sub) codec

------------------------------------------------------------------------------------------------------------------------

type Check :: ∀ {extt} {extq} . [Dd extt] -> Dd extq -> Constraint
class Check tables s where
  checked :: SqelFor tag s -> Fragment ('Frag ('Frag0 tag sort s root comp))

instance (
    s ~ 'Dd ext a sub,
    table ~ MkTableInfo a (DdTableName s) (table0 : tables),
    Node table s
  ) => Check (table0 : tables) ('Dd ext a sub) where
    checked s = Fragment (node @table s)

-- instance (
--     s ~ 'Dd ext a sub,
--     table ~ 'IsTable (DdTableName s),
--     Node table s
--   ) => Check '[] ('Dd ext a sub) where
--     checked s = Fragment (node @table s)

type Check1 table = Check '[table]

-- TODO maybe there should be additional types that distinguish tables from queries.
-- SqelQuery and SqelTable, corresponding to QuerySchema and TableSchema
unchecked ::
  ∀ tag s sort .
  SqelFor tag s ->
  Fragment ('Frag ('Frag0 tag sort s 'True (IsComp s)))
unchecked =
  Fragment
