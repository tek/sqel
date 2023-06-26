module Sqel.SOP.NP where

import Data.Some (Some (Some))
import qualified Fcf.Class.Monoid as Fcf
import Generics.SOP (AllN, HTraverse_, K (K), NP (Nil, (:*)), SListI, Top, hcfoldMap, hcollapse, hmap)

import Sqel.Kind.List (type (++))

appendNP ::
  ∀ {k} (as :: [k]) (bs :: [k]) f .
  NP f as ->
  NP f bs ->
  NP f (as ++ bs)
appendNP Nil bs =
  bs
appendNP (h :* t) bs =
  h :* appendNP t bs

appendNPFcf ::
  ∀ {k} (as :: [k]) (bs :: [k]) f .
  NP f as ->
  NP f bs ->
  NP f (as Fcf.<> bs)
appendNPFcf Nil bs =
  bs
appendNPFcf (h :* t) bs =
  h :* appendNPFcf t bs

class ReverseNPAcc acc as rev | acc as -> rev where
  reverseNPAcc :: NP f acc -> NP f as -> NP f rev

instance ReverseNPAcc acc '[] acc where
  reverseNPAcc !acc Nil = acc

instance ReverseNPAcc (a : acc) as rev => ReverseNPAcc acc (a : as) rev where
  reverseNPAcc !acc (a :* as) = reverseNPAcc (a :* acc) as

class ReverseNP as rev | as -> rev where
  reverseNP :: NP f as -> NP f rev

instance ReverseNPAcc '[] as rev => ReverseNP as rev where
  reverseNP = reverseNPAcc Nil

someNP ::
  SListI as =>
  NP f as ->
  [Some f]
someNP np =
  hcollapse (hmap (K . Some) np)

hfoldMap ::
  Monoid m =>
  HTraverse_ h =>
  AllN h Top xs =>
  (∀ x . f x -> m) ->
  h f xs ->
  m
hfoldMap f =
  hcfoldMap (Proxy @Top) f

hcmapList ::
  ∀ c h f xs a .
  AllN h c xs =>
  HTraverse_ h =>
  (∀ x . c x => f x -> a) ->
  h f xs ->
  [a]
hcmapList f = hcfoldMap (Proxy @c) (pure . f)

hmapList ::
  ∀ h f xs a .
  AllN h Top xs =>
  HTraverse_ h =>
  (∀ x . f x -> a) ->
  h f xs ->
  [a]
hmapList f = hcmapList @Top f
