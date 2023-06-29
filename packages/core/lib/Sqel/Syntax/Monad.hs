module Sqel.Syntax.Monad where

import Generics.SOP (NP (Nil))

import Sqel.Data.Clause (AppendClauses, Clauses (Clauses), appendClauses)
import Sqel.Data.Statement (Statement)
import Sqel.Kind.List (type (++))
import Sqel.Kind.Maybe (MaybeD (NothingD))
import Sqel.Syntax.Result (DoResult (doResult))

-- Functor

fmap :: (a -> b) -> Clauses tag cs results a -> Clauses tag cs results b
fmap = Prelude.fmap

-- Applicative

pure :: a -> Clauses tag '[] 'Nothing a
pure a = Clauses Nil NothingD a

(<*>) ::
  AppendClauses resl resr results =>
  Clauses tag csl resl (a -> b) ->
  Clauses tag csr resr a ->
  Clauses tag (csl ++ csr) results b
l@(Clauses _ _ f) <*> r =
  f <$> appendClauses l r

join ::
  AppendClauses resl resr results =>
  Clauses tag csl resl (Clauses tag csr resr a) ->
  Clauses tag (csl ++ csr) results a
join l@(Clauses _ _ r) = appendClauses l r

-- Monad

return :: a -> Clauses tag '[] 'Nothing a
return a = Clauses Nil NothingD a

(>>=) ::
  ∀ tag frags cs results result q a .
  DoResult frags tag cs results a q result =>
  Clauses tag '[] 'Nothing frags ->
  (frags -> Clauses tag cs results a) ->
  Statement q result
Clauses Nil NothingD frags >>= f =
  doResult @frags frags (f frags)

(>>) ::
  AppendClauses resl resr results =>
  Clauses tag csl resl a ->
  Clauses tag csr resr b ->
  Clauses tag (csl ++ csr) results b
(>>) = appendClauses
