module Sqel.Kind.HasFieldK where

import Sqel.Class.DdField (DdKFieldT)
import Sqel.Data.Dd (DdK (Dd), StructWith (Comp))

type HasFieldK :: Symbol -> k -> k
type family HasFieldK field a

type instance HasFieldK @(DdK _) field ('Dd _ _ ('Comp _ _ _ sub)) = DdKFieldT field sub

type (.) :: k -> Symbol -> k
type family (.) a field where
  (.) a field = HasFieldK field a