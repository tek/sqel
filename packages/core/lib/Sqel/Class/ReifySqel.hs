module Sqel.Class.ReifySqel where

import Generics.SOP (All, NP (Nil), hcpure)

import Sqel.Class.DdVal (DdMap (ddmap))
import Sqel.Class.ReifyCodec (CompCodec (compCodec), ReifyCodec (reifyCodec))
import Sqel.Class.ReifyComp (DemoteSort (demoteSort), ReifyComp (reifyComp))
import Sqel.Class.ReifyPrim (ReifyPrim (reifyPrim))
import Sqel.Data.Class.Dd (SingInc (singInc))
import Sqel.Data.Codec (FullCodec)
import Sqel.Data.Dd (DdK (Dd), StructWith (Comp, Prim))
import Sqel.Data.Def (Def)
import Sqel.Data.IndexState (IndexState, indexState)
import Sqel.Data.PgTypeName (PgTableName, pgTableName)
import Sqel.Data.Sel (TSelName)
import Sqel.Data.Sqel (SqelFor (SqelComp, SqelPrim), sqelCodec)
import Sqel.Dd (DdTableName, ExtMods)
import Sqel.SOP.Constraint (symbolText)
import Sqel.SOP.NP (hcpureA)

type Node :: ∀ {ext} . Bool -> Type -> DdK ext -> Constraint
class Node root tag s where
  node :: PgTableName -> IndexState (SqelFor tag s)

instance (
    s ~ 'Dd ext a ('Prim prim),
    ReifyPrim tag ext a prim,
    ReifyCodec FullCodec s
  ) => Node root tag ('Dd ext a ('Prim prim)) where
    node table = do
      meta <- reifyPrim @tag @ext @a @prim table
      pure (SqelPrim meta (reifyCodec @_ @s))

instance (
    s ~ 'Dd ext a ('Comp tsel c i sub),
    tname ~ TSelName tsel,
    mods ~ ExtMods ext,
    SingInc i,
    DdMap (SqelFor tag) FullCodec sub as,
    ReifyComp tag root ext a tsel c i,
    CompCodec c FullCodec a as,
    All (Node 'False tag) sub,
    DemoteSort tag c tname ext
  ) => Node root tag ('Dd ext a ('Comp tsel c i sub)) where
    node table = do
      compSort <- demoteSort @tag @c @tname @ext table
      sub <- hcpureA @(Node 'False tag) (node @'False table)
      let codec = compCodec @c (ddmap sqelCodec sub)
      pure (SqelComp (singInc @i) meta compSort sub codec)
      where
        meta = reifyComp @tag @root @ext @a @tsel @c @i table

------------------------------------------------------------------------------------------------------------------------

type ReifySqelFor :: ∀ {ext} . Type -> DdK ext -> Constraint
class ReifySqelFor tag s where
  reifySqel :: SqelFor tag s

instance (
    s ~ 'Dd ext a sub,
    table ~ DdTableName s,
    KnownSymbol table,
    Node 'True tag s
  ) => ReifySqelFor tag ('Dd ext a sub) where
    reifySqel =
      indexState (node @'True (pgTableName (symbolText @table)))

type ReifySqel = ReifySqelFor Def

sqel ::
  ∀ s tag .
  ReifySqelFor tag s =>
  SqelFor tag s
sqel = reifySqel

type ReifySqels :: ∀ {ext} . Type -> [DdK ext] -> Constraint
class ReifySqels tag ss where
  reifySqels :: NP (SqelFor tag) ss

instance ReifySqels tag '[] where
  reifySqels = Nil

instance (
    All (ReifySqelFor tag) (s : ss)
  ) => ReifySqels tag (s : ss) where
    reifySqels = hcpure (Proxy @(ReifySqelFor tag)) reifySqel

sqels ::
  ∀ s tag .
  ReifySqels tag s =>
  NP (SqelFor tag) s
sqels = reifySqels
