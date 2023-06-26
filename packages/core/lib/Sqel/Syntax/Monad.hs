module Sqel.Syntax.Monad where

import Generics.SOP (NP (Nil))

import Sqel.Data.Clause (Clauses (Clauses), appendClauses)
import Sqel.Kind.List (type (++))
import Sqel.SOP.NP (appendNP)
import Sqel.Syntax.Result (DoResult (doResult))

-- Monad

return :: a -> Clauses tag '[] a
return a = Clauses Nil a

(>>=) ::
  ∀ tag frags cs result a .
  DoResult frags tag cs a result =>
  Clauses tag '[] frags ->
  (frags -> Clauses tag cs a) ->
  result
Clauses Nil frags >>= f =
  doResult @frags frags (f frags)

(>>) ::
  Clauses tag csl a ->
  Clauses tag csr b ->
  Clauses tag (csl ++ csr) b
(>>) = appendClauses

-- Functor

fmap :: (a -> b) -> Clauses tag cs a -> Clauses tag cs b
fmap f (Clauses cs a) = Clauses cs (f a)

-- Applicative

pure :: a -> Clauses tag '[] a
pure a = Clauses Nil a

(<*>) ::
  Clauses tag csl (a -> b) ->
  Clauses tag csr a ->
  Clauses tag (csl ++ csr) b
Clauses csl f <*> Clauses csr a =
  Clauses (appendNP csl csr) (f a)

join ::
  Clauses tag csl (Clauses tag csr a) ->
  Clauses tag (csl ++ csr) a
join (Clauses csl (Clauses csr a)) = Clauses (appendNP csl csr) a
