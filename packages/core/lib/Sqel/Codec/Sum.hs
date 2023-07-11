module Sqel.Codec.Sum where

import Data.Functor.Contravariant.Divisible (choose)
import Data.Functor.Invariant (Invariant (invmap))
import Exon (exon)
import Generics.SOP (
  All,
  All2,
  HIndex (hindex),
  I,
  NP (Nil, (:*)),
  NS (S, Z),
  SListI,
  SListI2,
  SOP (SOP),
  Top,
  hcfoldMap,
  hcmap,
  hctraverse_,
  hsequence,
  unSOP,
  )
import Generics.SOP.GGP (gfrom, gto)
import Hasql.Decoders (Row)
import Hasql.Encoders (Params)

import Sqel.Class.ReifyPrimCodec (reifyPrimCodec)
import Sqel.Codec.Product (GetDecoder (getDecoder), GetEncoder (getEncoder), prodParams)
import qualified Sqel.Data.Codec as Codec
import Sqel.Data.Codec (Codec (Codec), Decoder (Decoder), Encoder (Encoder), FullCodec)
import Sqel.Data.Dd (ConCol (ConCol), ConColF (ConColF, conColF))
import Sqel.Data.Mods (NoMods)
import Sqel.SOP.Constraint (ConstructSOP, ReifySOP)

unconsNS ::
  NS (NP I) (ds : dss) ->
  Either (NP I ds) (NS (NP I) dss)
unconsNS = \case
  Z x -> Left x
  S x -> Right x

readNulls ::
  ∀ ass .
  SListI2 ass =>
  NP (ConColF Decoder) ass ->
  Row ()
readNulls cons =
  hctraverse_ (Proxy @SListI) (\ (ConColF d) -> d.decodeNulls) cons

sumRows ::
  All2 Top ass =>
  NP (ConColF Decoder) ass ->
  Int64 ->
  Row (NS (NP I) ass)
sumRows (ConColF con :* cons) 0 =
  Z <$> con.decodeValue <* readNulls cons
sumRows (ConColF con :* cons) index = do
  con.decodeNulls
  S <$> sumRows cons (index - 1)
sumRows Nil index =
  fail [exon|invalid index into sum type in database: #{show index}|]

writeNull ::
  ∀ a as .
  ConColF Encoder as ->
  Params a
writeNull (ConColF enc) =
  unit >$< enc.encodeNulls

writeNulls ::
  ∀ a ass .
  SListI2 ass =>
  NP (ConColF Encoder) ass ->
  Params a
writeNulls =
  hcfoldMap (Proxy @SListI) writeNull

sumParams ::
  SListI2 ass =>
  NP (ConColF Encoder) ass ->
  Params (NS (NP I) ass)
sumParams = \case
  con :* cons ->
    choose unconsNS inhabited uninhabited
    where
      inhabited = con.conColF.encodeValue <> writeNulls cons
      uninhabited = writeNull con <> sumParams cons
  Nil ->
    mempty

type WrapCons :: (Type -> Type) -> [[Type]] -> [Type] -> Constraint
class WrapCons b ass as | ass -> as where
  wrapCons :: NP b as -> NP (ConColF b) ass

instance WrapCons b '[] '[] where
  wrapCons Nil = Nil

instance (
    Invariant b,
    WrapCons b ass as
  ) => WrapCons b (as' : ass) (ConCol as' : as) where
    wrapCons (b :* bs) =
      ConColF (invmap coerce coerce b) :* wrapCons bs

indexDecoder :: Decoder Int64
indexDecoder = reifyPrimCodec @NoMods

indexEncoder :: Encoder Int64
indexEncoder = reifyPrimCodec @NoMods

decodeValue ::
  ReifySOP a ass =>
  NP (ConColF Decoder) ass ->
  Row a
decodeValue decoders =
  gto . SOP <$> (sumRows decoders =<< indexDecoder.decodeValue)

encodeValue ::
  ConstructSOP a ass =>
  NP (ConColF Encoder) ass ->
  Params a
encodeValue encoders =
  unSOP . gfrom >$< (index <> sumParams encoders)
  where
    index = fromIntegral . hindex >$< indexEncoder.encodeValue

type GetConEncoder :: (Type -> Type) -> [Type] -> Constraint
class GetConEncoder b as where
  getConEncoder :: ConColF b as -> ConColF Encoder as

instance GetConEncoder FullCodec a where
  getConEncoder (ConColF c) = ConColF c.encoder

instance GetConEncoder Encoder a where
  getConEncoder = id

type GetConDecoder :: (Type -> Type) -> [Type] -> Constraint
class GetConDecoder b as where
  getConDecoder :: ConColF b as -> ConColF Decoder as

instance GetConDecoder FullCodec a where
  getConDecoder (ConColF c) = ConColF c.decoder

instance GetConDecoder Decoder a where
  getConDecoder = id

type SumEncoder :: (Type -> Type) -> Type -> [[Type]] -> Constraint
class SumEncoder b a ass | a -> ass where
  sumEncoder :: NP (ConColF b) ass -> Encoder a

instance (
    ConstructSOP a ass,
    All (GetConEncoder b) ass
  ) => SumEncoder b a ass where
    sumEncoder conCodecs =
      Encoder (encodeValue encoders) (indexEncoder.encodeNulls <> writeNulls encoders)
      where
        encoders = hcmap (Proxy @(GetConEncoder b)) getConEncoder conCodecs

type SumDecoder :: (Type -> Type) -> Type -> [[Type]] -> Constraint
class SumDecoder b a ass | a -> ass where
  sumDecoder :: NP (ConColF b) ass -> Decoder a

instance (
    ReifySOP a ass,
    All (GetConDecoder b) ass
  ) => SumDecoder b a ass where
    sumDecoder conCodecs =
      Decoder (decodeValue decoders) (indexDecoder.decodeNulls *> readNulls decoders)
      where
        decoders = hcmap (Proxy @(GetConDecoder b)) getConDecoder conCodecs

type SumCodec :: (Type -> Type) -> Type -> [Type] -> Constraint
class SumCodec b a as | a -> as where
  sumCodec :: NP b as -> b a

instance (
    WrapCons FullCodec ass as,
    SumDecoder FullCodec a ass,
    SumEncoder FullCodec a ass
  ) => SumCodec FullCodec a as where
    sumCodec conCodecs =
      Codec {
        decoder = sumDecoder wrapped,
        encoder = sumEncoder wrapped
      }
      where
        wrapped = wrapCons conCodecs

instance (
    WrapCons Encoder ass as,
    SumEncoder Encoder a ass
  ) => SumCodec Encoder a as where
    sumCodec = sumEncoder . wrapCons

instance (
    WrapCons Decoder ass as,
    SumDecoder Decoder a ass
  ) => SumCodec Decoder a as where
    sumCodec = sumDecoder . wrapCons

type ConEncoder :: (Type -> Type) -> [Type] -> Constraint
class ConEncoder b as where
  conEncoder :: NP b as -> Encoder (ConCol as)

instance All (GetEncoder b) as => ConEncoder b as where
  conEncoder cs = coerce (prodParams (hcmap (Proxy @(GetEncoder b)) getEncoder cs))

type ConDecoder :: (Type -> Type) -> [Type] -> Constraint
class ConDecoder b as where
  conDecoder :: NP b as -> Decoder (ConCol as)

instance All (GetDecoder b) as => ConDecoder b as where
  conDecoder cs = ConCol <$> hsequence (hcmap (Proxy @(GetDecoder b)) getDecoder cs)

type ConCodec :: (Type -> Type) -> [Type] -> Constraint
class ConCodec b as where
  conCodec :: NP b as -> b (ConCol as)

instance (
    ConDecoder FullCodec as,
    ConEncoder FullCodec as
  ) => ConCodec FullCodec as where
    conCodec cs =
      Codec {
        decoder = conDecoder cs,
        encoder = conEncoder cs
      }

instance ConEncoder Encoder as => ConCodec Encoder as where
  conCodec = conEncoder

instance ConDecoder Decoder as => ConCodec Decoder as where
  conCodec = conDecoder
