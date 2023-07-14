module Sqel.Class.Check where

import Generics.SOP (All, hcmap)

import Sqel.Data.Dd (Dd (Dd), PrimType (Cond, NoCond), Struct (Comp, Prim))
import Sqel.Data.Fragment (Frag (Frag), Frag0 (Frag0), Fragment (Fragment))
import Sqel.Data.PgTypeName (PgTableName, pgTableName)
import Sqel.Data.Sel (Paths)
import Sqel.Data.Sqel (SqelFor (SqelPrim), pattern SqelMerge, pattern SqelNest)
import Sqel.Dd (ExtPath, IsComp)
import Sqel.Kind.CheckPrim (CheckPrim, MkTableFPaths, TableFPaths)
import Sqel.SOP.Constraint (symbolText)

type PrimTable :: ∀ {ext} . [TableFPaths ext] -> Paths -> Type -> Constraint
class PrimTable paths path a where
  primTable :: PgTableName

instance (
    table ~ CheckPrim paths a path,
    KnownSymbol table
  ) => PrimTable paths path a where
    primTable = pgTableName (symbolText @table)

------------------------------------------------------------------------------------------------------------------------

type Node :: ∀ {ext} . [TableFPaths ext] -> Dd ext -> Constraint
class Node paths s where
  node :: SqelFor tag s -> SqelFor tag s

instance (
    path ~ ExtPath ext,
    PrimTable paths path a
  ) => Node paths ('Dd ext a ('Prim 'Cond)) where
    node (SqelPrim _ p codec) =
      SqelPrim (primTable @paths @path @a) p codec

instance Node paths ('Dd ext a ('Prim 'NoCond)) where
  node = id

instance All (Node paths) sub => Node paths ('Dd ext a ('Comp tsel c i sub)) where
  node = \case
    SqelNest c compSort sub codec ->
      SqelNest c compSort (hcmap (Proxy @(Node paths)) (node @paths) sub) codec
    SqelMerge c compSort sub codec ->
      SqelMerge c compSort (hcmap (Proxy @(Node paths)) (node @paths) sub) codec

------------------------------------------------------------------------------------------------------------------------

type Check :: ∀ {extt} {extq} . [Dd extt] -> Dd extq -> Constraint
class Check tables s where
  checked :: SqelFor tag s -> Fragment ('Frag ('Frag0 tag sort s root comp))

instance (
    paths ~ MkTableFPaths (table0 : tables),
    Node paths ('Dd ext a sub)
  ) => Check (table0 : tables) ('Dd ext a sub) where
    checked s = Fragment (node @paths s)

type Check1 table = Check '[table]

unchecked ::
  ∀ tag s sort .
  SqelFor tag s ->
  Fragment ('Frag ('Frag0 tag sort s 'True (IsComp s)))
unchecked =
  Fragment
