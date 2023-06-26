module Sqel.Class.ReifyCodec where

import Generics.SOP (NP)
import Hasql.Decoders (Row)
import Hasql.Encoders (Params)

import Sqel.Class.DdVal (DdPure (ddPure), DdVal (DdVal))
import Sqel.Class.ReifyPrimCodec (ReifyPrimCodec (reifyPrimCodec))
import Sqel.Codec.Product (ProdCodec (prodCodec))
import Sqel.Codec.Sum (ConCodec (conCodec), SumCodec (sumCodec))
import qualified Sqel.Data.Codec as Codec
import Sqel.Data.Codec (Decoder, Encoder)
import Sqel.Data.Dd (ConCol, DdK (Dd), Sort (Con, Prod, Sum), StructWith (Comp, Prim))
import Sqel.Dd (DdType, DdTypes, ExtMods)

type CompCodec :: Sort -> (Type -> Type) -> Type -> [Type] -> Constraint
class CompCodec c b a as | a -> as where
  compCodec :: NP b as -> b a

instance ProdCodec b a as => CompCodec 'Prod b a as where
  compCodec = prodCodec

instance ConCodec b as => CompCodec 'Con b (ConCol as) as where
  compCodec = conCodec

instance SumCodec b a as => CompCodec ('Sum prefix) b a as where
  compCodec = sumCodec

type ReifyCodec :: ∀ {ext} . (Type -> Type) -> DdK ext -> Constraint
class ReifyCodec b s where
  reifyCodec :: b (DdType s)

instance (
    mods ~ ExtMods ext,
    ReifyPrimCodec mods b a
  ) => ReifyCodec b ('Dd ext a ('Prim prim)) where
    reifyCodec = reifyPrimCodec @mods

instance (
    mods ~ ExtMods ext,
    DdTypes sub ~ as,
    DdPure (ReifyCodec b) sub b as,
    CompCodec c b a as
  ) => ReifyCodec b ('Dd ext a ('Comp tsel c i sub)) where
    reifyCodec =
      compCodec @c (ddPure @(ReifyCodec b) @sub rc)
      where
        rc :: ∀ s . ReifyCodec b s => DdVal b s
        rc = DdVal (reifyCodec @b @s)

reifyParams :: ∀ s . ReifyCodec Encoder s => Params (DdType s)
reifyParams = (reifyCodec @Encoder @s).encodeValue

reifyRow :: ∀ s . ReifyCodec Decoder s => Row (DdType s)
reifyRow = (reifyCodec @Decoder @s).decodeValue
