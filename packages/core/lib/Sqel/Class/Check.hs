module Sqel.Class.Check where

import Generics.SOP (All, hcmap)

import Sqel.Data.Dd (DdK (Dd), PrimType (Cond, NoCond), StructWith (Comp, Prim))
import Sqel.Data.Fragment (Frag (Frag), Frag0 (Frag0), Fragment (Fragment))
import Sqel.Data.PgTypeName (pgTableName)
import Sqel.Data.Sel (Paths)
import Sqel.Data.Sqel (SqelFor (SqelPrim), pattern SqelMerge, pattern SqelNest)
import Sqel.Dd (DdTableName, ExtPath, IsComp)
import Sqel.Data.Def (Def)
import qualified Sqel.Default as PrimMeta (PrimMeta (table))
import Sqel.Kind.CheckPrim (CheckPrim)
import Sqel.Kind.TableInfo (MkTableInfo, TableInfo (AvailTables, IsTable))
import Sqel.SOP.Constraint (symbolText)

type PrimTable :: TableInfo -> Paths -> Type -> Constraint
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

type Node :: ∀ {ext} . TableInfo -> Type -> DdK ext -> Constraint
class Node table tag s where
  node :: SqelFor tag s -> SqelFor tag s

instance (
    path ~ ExtPath ext,
    PrimTable table path a
  ) => Node table Def ('Dd ext a ('Prim 'Cond)) where
    node (SqelPrim p codec) =
      SqelPrim p { PrimMeta.table = pgTableName (primTable @table @path @a) } codec

instance Node table Def ('Dd ext a ('Prim 'NoCond)) where
  node = id

instance (
    All (Node table tag) sub
  ) => Node table tag ('Dd ext a ('Comp tsel c i sub)) where
    node = \case
      SqelNest c compSort sub codec ->
        SqelNest c compSort (hcmap (Proxy @(Node table tag)) (node @table) sub) codec
      SqelMerge c compSort sub codec ->
        SqelMerge c compSort (hcmap (Proxy @(Node table tag)) (node @table) sub) codec

------------------------------------------------------------------------------------------------------------------------

type Checked :: ∀ {extt} {extq} . [DdK extt] -> Type -> DdK extq -> Constraint
class Checked tables tag s where
  checked :: SqelFor tag s -> Fragment ('Frag ('Frag0 tag sort s root comp))

-- TODO this passes @a@ and @DdTableName s@ to @MkTableInfo@ for hypothetical performance improvements, but that has the
-- side effect that the @table@ combinator for Sqel.Do requires @Check '[t] t@ for polymorphic @t@, which would probably
-- be detected if we'd pass @s@ and match on the entire Dd.
--
-- However, we can't match on @s@ because the matched thing may be a projection? not sure, at that point we're only
-- dealing with roots usually, although one could use a projection for a query type, like when inserting values with
-- defaults or something.
--
-- An alternative would be to mark projections and fork the check based on that.
-- Which also doesn't help when the Dd is poly.
instance (
    s ~ 'Dd ext a sub,
    table ~ MkTableInfo a (DdTableName s) (table0 : tables),
    Node table tag s
  ) => Checked (table0 : tables) tag ('Dd ext a sub) where
    checked s = Fragment (node @table s)

instance (
    s ~ 'Dd ext a sub,
    table ~ 'IsTable (DdTableName s),
    Node table tag s
  ) => Checked '[] tag ('Dd ext a sub) where
    checked s = Fragment (node @table s)

type Checked1 table tag = Checked '[table] tag

-- TODO proxy Checked with a class that triages the tag to get the error to say Check instead of Checked Def
type Check tables = Checked tables Def

type Check1 table = Check '[table]

-- TODO maybe there should be additional types that distinguish tables from queries.
-- SqelQuery and SqelTable, corresponding to QuerySchema and TableSchema
unchecked ::
  ∀ tag s sort .
  SqelFor tag s ->
  Fragment ('Frag ('Frag0 tag sort s 'True (IsComp s)))
unchecked =
  Fragment
