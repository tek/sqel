module Sqel.Data.Mods.Newtype where

import Sqel.Class.ReifyDecoder (ReifyDecoder (reifyDecoder))
import Sqel.Class.ReifyEncoder (ReifyEncoder (reifyEncoder))
import Sqel.SOP.Newtype (UnwrapNewtype (unwrapNewtype, wrapNewtype))

type Newtype :: Type -> Type -> Type
data Newtype a w

instance (
    UnwrapNewtype a w,
    ReifyEncoder mods w
  ) => ReifyEncoder (Newtype a w : mods) a where
    reifyEncoder =
      unwrapNewtype >$< reifyEncoder @mods

instance (
    UnwrapNewtype a w,
    ReifyDecoder mods w
  ) => ReifyDecoder (Newtype a w : mods) a where
    reifyDecoder =
      wrapNewtype <$> reifyDecoder @mods
