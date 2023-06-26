module Sqel.Kind.Maybe where

type MaybeD :: (k -> Type) -> Maybe k -> Type
data MaybeD f k where
  JustD :: f a -> MaybeD f ('Just a)
  NothingD :: MaybeD f 'Nothing

type MaybeC :: ∀ k . (k -> Constraint) -> Maybe k -> Constraint
type family MaybeC c k where
  MaybeC _ 'Nothing = ()
  MaybeC c ('Just a) = c a
