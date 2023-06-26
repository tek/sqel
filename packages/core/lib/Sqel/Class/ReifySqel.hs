module Sqel.Class.ReifySqel where

import Generics.SOP (All, NP (Nil), hcpure)

import Sqel.Class.DdVal (DdMap (ddmap))
import Sqel.Class.ReifyCodec (CompCodec (compCodec), ReifyCodec (reifyCodec))
import Sqel.Class.ReifyComp (DemoteSort (demoteSort), ReifyComp (reifyComp))
import Sqel.Class.ReifyPrim (ReifyPrim (reifyPrim))
import Sqel.Data.Class.Dd (SingInc (singInc))
import Sqel.Data.Codec (FullCodec)
import Sqel.Data.Dd (Dd, DdK (Dd), SInc (SMerge, SNest), StructWith (Comp, Prim))
import Sqel.Data.PgTypeName (PgTableName, pgTableName)
import Sqel.Data.Sel (TSelName)
import Sqel.Data.Sqel (SqelFor (SqelPrim), pattern SqelMerge, pattern SqelNest, sqelCodec)
import Sqel.Dd (DdTableName, DdTypes, ExtMods)
import Sqel.Default (Def)
import Sqel.SOP.Constraint (symbolText)

type Node :: ∀ {ext} . Bool -> Type -> DdK ext -> Constraint
class Node root tag s where
  node :: PgTableName -> SqelFor tag s

instance (
    s ~ 'Dd ext a ('Prim prim),
    ReifyPrim tag ext a prim,
    ReifyCodec FullCodec s
  ) => Node root tag ('Dd ext a ('Prim prim)) where
    node table =
      SqelPrim meta codec
      where
        meta = reifyPrim @tag @ext @a @prim table
        codec = reifyCodec @_ @s

instance (
    s ~ 'Dd ext a ('Comp tsel c i sub),
    tname ~ TSelName tsel,
    mods ~ ExtMods ext,
    SingInc i,
    DdMap (SqelFor tag) FullCodec sub,
    ReifyComp tag root ext a tsel c i,
    CompCodec c FullCodec a (DdTypes sub),
    All (Node 'False tag) sub,
    DemoteSort tag c tname ext
  ) => Node root tag ('Dd ext a ('Comp tsel c i sub)) where
    node table =
      case singInc @i of
        SNest -> SqelNest meta compSort sub codec
        SMerge -> SqelMerge meta compSort sub codec
      where
        meta = reifyComp @tag @root @ext @a @tsel @c @i table
        codec :: FullCodec a
        codec = compCodec @c (ddmap sqelCodec sub)
        sub :: NP (SqelFor tag) sub
        sub = hcpure (Proxy @(Node 'False tag)) (node @'False table)
        compSort = demoteSort @tag @c @tname @ext table

------------------------------------------------------------------------------------------------------------------------

type ReifySqel :: ∀ {ext} . Type -> DdK ext -> Constraint
class ReifySqel tag s where
  reifySqel :: SqelFor tag s

instance (
    s ~ 'Dd ext a sub,
    table ~ DdTableName s,
    KnownSymbol table,
    Node 'True tag s
  ) => ReifySqel tag ('Dd ext a sub) where
    reifySqel =
      node @'True (pgTableName (symbolText @table))

type ReifySqelDef = ReifySqel Def

sqel ::
  ∀ s tag .
  ReifySqel tag s =>
  SqelFor tag s
sqel = reifySqel

type ReifySqels :: Type -> [Dd] -> Constraint
class ReifySqels tag ss where
  reifySqels :: NP (SqelFor tag) ss

instance ReifySqels tag '[] where
  reifySqels = Nil

instance (
    All (ReifySqel tag) (s : ss)
  ) => ReifySqels tag (s : ss) where
  reifySqels = hcpure (Proxy @(ReifySqel tag)) reifySqel

sqels ::
  ∀ s tag .
  ReifySqels tag s =>
  NP (SqelFor tag) s
sqels = reifySqels
