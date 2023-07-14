module Sqel.Kind.Project where

import Sqel.Class.DdField (DdFieldT)
import Sqel.Data.Dd (Dd (Dd), Struct (Comp))

type Project :: Symbol -> k -> k
type family Project field a

type instance Project @(Dd _) field ('Dd _ _ ('Comp _ _ _ sub)) = DdFieldT field sub

type (.) :: k -> Symbol -> k
type family (.) a field where
  (.) a field = Project field a
