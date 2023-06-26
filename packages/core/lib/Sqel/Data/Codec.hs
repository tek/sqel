module Sqel.Data.Codec where

import Data.Functor.Invariant (Invariant (invmap), invmapContravariant)
import Exon (exon)
import Hasql.Decoders (Row)
import Hasql.Encoders (Params)

import Sqel.SOP.Constraint (TryDataName (tryDataName))

data Encoder a =
  Encoder {
    encodeValue :: Params a,
    encodeNulls :: Params ()
  }
  deriving stock (Generic)

instance Semigroup (Encoder a) where
  Encoder vl nl <> Encoder vr nr =
    Encoder (vl <> vr) (nl <> nr)

instance Monoid (Encoder a) where
  mempty =
    Encoder mempty mempty

instance Contravariant Encoder where
  contramap f Encoder {..} =
    Encoder (f >$< encodeValue) encodeNulls

instance Invariant Encoder where
  invmap = invmapContravariant

data Decoder a =
  Decoder {
    decodeValue :: Row a,
    decodeNulls :: Row ()
  }
  deriving stock (Generic)

instance Functor Decoder where
  fmap f Decoder {..} =
    Decoder (f <$> decodeValue) decodeNulls

instance Applicative Decoder where
  pure a =
    Decoder (pure a) (pure ())

  liftA2 f (Decoder vl nl) (Decoder vr nr) =
    Decoder (liftA2 f vl vr) (nl *> nr)

data Codec e d a =
  Codec {
    encoder :: e a,
    decoder :: d a
  }
  deriving stock (Generic)

instance (
    TryDataName a
  ) => Show (Codec e d a) where
    showsPrec d _ =
      showParen (d > 10) [exon|Codec @#{showString (toString (tryDataName @a))}|]

instance Eq (Codec e d a) where
  _ == _ = True

instance (
    Contravariant e,
    Functor d
  ) => Invariant (Codec e d) where
    invmap fd fe Codec {..} =
      Codec {
        encoder = fe >$< encoder,
        decoder = fd <$> decoder
      }

type FullCodec = Codec Encoder Decoder
