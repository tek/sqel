module Sqel.Data.Mods.CustomCodec where

import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders

import Sqel.Class.ReifyDecoder (ReifyDecoder (reifyDecoder))
import Sqel.Class.ReifyEncoder (ReifyEncoder (reifyEncoder))

data CustomCodec tag a

type CustomEncoder :: Type -> [Type] -> Type -> Constraint
class CustomEncoder tag mods a where
  customEncoder :: Encoders.Value a

instance CustomEncoder tag mods a => ReifyEncoder (CustomCodec tag a : mods) a where
  reifyEncoder = customEncoder @tag @mods

type CustomDecoder :: Type -> [Type] -> Type -> Constraint
class CustomDecoder tag mods a where
  customDecoder :: Decoders.Value a

instance CustomDecoder tag mods a => ReifyDecoder (CustomCodec tag a : mods) a where
  reifyDecoder = customDecoder @tag @mods
