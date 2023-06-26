{-# language UndecidableSuperClasses #-}

module Sqel.SOP.Fundep where

import Generics.SOP (AllZip, NP, htrans)

class c a b => Fundep (c :: k -> l -> Constraint) (a :: k) (b :: l) | c a -> b where

class AllZip c as bs => Fundeps (c :: k -> l -> Constraint) (as :: [k]) (bs :: [l]) | c as -> bs where

instance Fundeps c '[] '[] where

instance (
    Fundep c a b,
    Fundeps c as bs
  ) => Fundeps c (a : as) (b : bs) where

fdtrans ::
  ∀ c f g as bs .
  Fundeps c as bs =>
  (∀ a b . c a b => f a -> g b) ->
  NP f as ->
  NP g bs
fdtrans f =
  htrans (Proxy @c) f
{-# inline fdtrans #-}
