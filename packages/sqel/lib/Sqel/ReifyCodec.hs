module Sqel.ReifyCodec where

import Data.Functor.Invariant (Invariant, invmap)
import Generics.SOP (I (I), NP (Nil, (:*)))
import qualified Hasql.Encoders as Encoders

import Sqel.Codec (
  ColumnDecoder (columnDecoder, columnDecoderNullable),
  ColumnEncoder (columnEncoder, columnEncoderIgnore, columnEncoderNullable),
  PrimColumn (primDecoder, primEncoder),
  fullPrimCodec,
  ignoreDecoder,
  )
import Sqel.Codec.PrimDecoder (ArrayDecoder (arrayDecoder), enumDecoder, readDecoder)
import Sqel.Codec.PrimEncoder (arrayEncoder)
import Sqel.Codec.Product (ProdCodec (prodCodec))
import Sqel.Codec.Sum (ConCodec (conCodec), SumCodec (sumCodec), ignoreEncoder)
import qualified Sqel.Data.Codec as Codec
import Sqel.Data.Codec (Codec (Codec), Decoder (Decoder), Encoder (Encoder), FullCodec, ValueCodec)
import Sqel.Data.Dd (
  Comp (Prod, Sum),
  CompInc,
  ConCol,
  Dd (Dd),
  DdK (DdK),
  DdStruct (DdComp, DdPrim),
  ProdType (Con, Reg),
  Struct (Comp, Prim),
  )
import Sqel.Data.Mods (
  ArrayColumn (ArrayColumn),
  EnumColumn (EnumColumn),
  Ignore (Ignore),
  Mods (Mods),
  Newtype (Newtype),
  Nullable (Nullable),
  ReadShowColumn (ReadShowColumn),
  )
import Sqel.Mods (PrimCodec (PrimCodec), PrimValueCodec, PrimValueEncoder)
import Sqel.SOP.Enum (EnumTable)

type CompCodec :: Comp -> CompInc -> Type -> (Type -> Type) -> [Type] -> Constraint
class CompCodec c i a b as where
  compCodec :: NP b as -> b a

instance (
    ProdCodec b a as
  ) => CompCodec ('Prod 'Reg) i a b as where
    compCodec = prodCodec

instance (
    ConCodec b as
  ) => CompCodec ('Prod ('Con as)) i (ConCol name record fields as) b as where
    compCodec = conCodec

instance (
    SumCodec b a as
  ) => CompCodec 'Sum i a b as where
    compCodec = sumCodec

class DefaultPrimCodec b a where
  defaultPrimCodec :: b a

instance (
    PrimColumn a
  ) => DefaultPrimCodec FullCodec a where
    defaultPrimCodec =
      Codec {
        encoder = Encoder (columnEncoder encoder) (columnEncoderIgnore encoder),
        decoder = Decoder (columnDecoder decoder) (void ignoreDecoder)
      }
      where
        encoder = primEncoder
        decoder = primDecoder

instance (
    PrimColumn a
  ) => DefaultPrimCodec ValueCodec a where
    defaultPrimCodec =
      Codec {
        encoder = primEncoder,
        decoder = primDecoder
      }

instance (
    PrimColumn a
  ) => DefaultPrimCodec Encoder a where
    defaultPrimCodec =
      Encoder (columnEncoder encoder) (columnEncoderIgnore encoder)
      where
        encoder = primEncoder

instance (
    PrimColumn a
  ) => DefaultPrimCodec Encoders.Value a where
    defaultPrimCodec = primEncoder

type DefaultCompCodec :: Comp -> CompInc -> (Type -> Type) -> Type -> [Type] -> Constraint
class DefaultCompCodec c i b a as where
  defaultCompCodec :: NP b as -> b a

instance (
    CompCodec c i a FullCodec as
  ) => DefaultCompCodec c i FullCodec a as where
    defaultCompCodec = compCodec @c @i

instance (
    CompCodec c i a Encoder as
  ) => DefaultCompCodec c i Encoder a as where
    defaultCompCodec = compCodec @c @i

class ReifyPrimCodec b ps a where
  reifyPrimCodec :: NP I ps -> b a

instance {-# overlappable #-} (
    ReifyPrimCodec b ps a
  ) => ReifyPrimCodec b (p : ps) a where
  reifyPrimCodec (_ :* ps) =
    reifyPrimCodec ps

instance ReifyPrimCodec ValueCodec (PrimValueCodec a : ps) a where
  reifyPrimCodec (I (PrimCodec c) :* _) = c

instance ReifyPrimCodec FullCodec (PrimValueCodec a : ps) a where
  reifyPrimCodec (I (PrimCodec (Codec encoder decoder)) :* _) =
    Codec {
      encoder = Encoder (columnEncoder encoder) (columnEncoderIgnore encoder),
      decoder = Decoder (columnDecoder decoder) (void ignoreDecoder)
    }

instance (
    ReifyPrimCodec ValueCodec ps a
  ) => ReifyPrimCodec FullCodec (Nullable : ps) (Maybe a) where
    reifyPrimCodec (I (Nullable _) :* ps) =
      Codec {
        encoder = Encoder (columnEncoderNullable encoder) (ignoreEncoder encoder),
        decoder = Decoder (columnDecoderNullable decoder) (void ignoreDecoder)
      }
      where
        Codec encoder decoder = reifyPrimCodec @ValueCodec ps

instance ReifyPrimCodec Encoders.Value (PrimValueEncoder a : ps) a where
  reifyPrimCodec (I (PrimCodec e) :* _) = e

instance ReifyPrimCodec Encoder (PrimValueEncoder a : ps) a where
  reifyPrimCodec (I (PrimCodec encoder) :* _) =
    Encoder (columnEncoder encoder) (columnEncoderIgnore encoder)

-- TODO this could also produce NullableOrNot
instance (
    ReifyPrimCodec Encoders.Value ps a
  ) => ReifyPrimCodec Encoder (Nullable : ps) (Maybe a) where
    reifyPrimCodec (I (Nullable _) :* ps) =
      Encoder (columnEncoderNullable encoder) (ignoreEncoder encoder)
      where
        encoder = reifyPrimCodec @Encoders.Value ps

instance (
    Show a,
    EnumTable a
  ) => ReifyPrimCodec FullCodec (EnumColumn : ps) a where
  reifyPrimCodec (I EnumColumn :* _) =
    fullPrimCodec (Encoders.enum show) enumDecoder

instance (
    Show a,
    EnumTable a
  ) => ReifyPrimCodec ValueCodec (EnumColumn : ps) a where
  reifyPrimCodec (I EnumColumn :* _) =
    Codec (Encoders.enum show) enumDecoder

instance (
    Show a,
    Read a
  ) => ReifyPrimCodec FullCodec (ReadShowColumn : ps) a where
  reifyPrimCodec (I ReadShowColumn :* _) =
    fullPrimCodec (Encoders.enum show) readDecoder

instance (
    Show a,
    Read a
  ) => ReifyPrimCodec ValueCodec (ReadShowColumn : ps) a where
  reifyPrimCodec (I ReadShowColumn :* _) =
    Codec (Encoders.enum show) readDecoder

instance (
    ReifyPrimCodec ValueCodec ps a,
    Foldable f,
    ArrayDecoder f a
  ) => ReifyPrimCodec ValueCodec (ArrayColumn f : ps) (f a) where
  reifyPrimCodec (I ArrayColumn :* ps) =
    Codec (arrayEncoder encoder) (arrayDecoder decoder)
      where
        Codec encoder decoder = reifyPrimCodec @ValueCodec ps

instance (
    ReifyPrimCodec ValueCodec ps a,
    Foldable f,
    ArrayDecoder f a
  ) => ReifyPrimCodec FullCodec (ArrayColumn f : ps) (f a) where
  reifyPrimCodec (I ArrayColumn :* ps) =
    fullPrimCodec (arrayEncoder encoder) (arrayDecoder decoder)
      where
        Codec encoder decoder = reifyPrimCodec @ValueCodec ps

instance {-# overlappable #-} (
    ReifyPrimCodec c mods w,
    Invariant c
  ) => ReifyPrimCodec c (Newtype a w : mods) a where
  reifyPrimCodec (I (Newtype unwrap wrap) :* mods) =
    invmap wrap unwrap (reifyPrimCodec mods)

instance (
    ReifyPrimCodec Encoders.Value mods w
  ) => ReifyPrimCodec Encoders.Value (Newtype a w : mods) a where
  reifyPrimCodec (I (Newtype unwrap _) :* mods) =
    unwrap >$< (reifyPrimCodec mods)

instance ReifyPrimCodec FullCodec (Ignore : ps) a where
  reifyPrimCodec (I Ignore :* _) =
    Codec (Encoder mempty mempty) (Decoder (fail err) (fail err))
    where
      err = "ignored column was used"

instance ReifyPrimCodec Encoder (Ignore : ps) a where
  reifyPrimCodec (I Ignore :* _) =
    Encoder mempty mempty

instance (
    DefaultPrimCodec b a
  ) => ReifyPrimCodec b '[] a where
    reifyPrimCodec _ = defaultPrimCodec

class ReifyCompCodec b c i ps as a where
  reifyCompCodec :: NP I ps -> NP b as -> b a

instance (
    DefaultCompCodec c i b a as
  ) => ReifyCompCodec b c i ps as a where
    reifyCompCodec _ sub =
      defaultCompCodec @c @i sub

type ReifyCodec :: (Type -> Type) -> DdK -> Type -> Constraint
class ReifyCodec b s a | s -> a where
  reifyCodec :: Dd s -> b a

instance (
    ReifyPrimCodec b ps a
  ) => ReifyCodec b ('DdK sel ps a 'Prim) a where
    reifyCodec (Dd _ (Mods ps) DdPrim) =
      reifyPrimCodec @b ps

instance (
    ReifyCodecComp b sub as,
    ReifyCompCodec b c i ps as a
  ) => ReifyCodec b ('DdK sel ps a ('Comp tsel c i sub)) a where
    reifyCodec (Dd _ (Mods ps) (DdComp _ _ _ sub)) =
      reifyCompCodec @b @c @i @ps @as ps (reifyCodecComp @b @sub sub)

-- TODO this is probably only necessary because of a bug in GHC that's fixed in master
type ReifyCodecComp :: (Type -> Type) -> [DdK] -> [Type] -> Constraint
class ReifyCodecComp b s as | s -> as where
  reifyCodecComp :: NP Dd s -> NP b as

instance ReifyCodecComp b '[] '[] where
  reifyCodecComp Nil = Nil

instance (
    ReifyCodec b s a,
    ReifyCodecComp b ss as
  ) => ReifyCodecComp b (s : ss) (a : as) where
  reifyCodecComp (d :* ds) = reifyCodec d :* reifyCodecComp ds
