module Sqel.Kind.ResultDds where

import Sqel.Data.ClauseConfig (ClauseResultFor)
import Sqel.Data.Dd (DdK)
import Sqel.Data.Fragment (Frag)
import Sqel.Kind.Fragment (FragsDds, FragsTypes)

type NonEmptyK :: [k] -> Maybe [k]
type family NonEmptyK s = r | r -> s where
  NonEmptyK '[] = 'Nothing
  NonEmptyK s = 'Just s

type ResultDds' :: ∀ {ext} . Bool -> [Frag ext] -> Maybe [DdK ext]
type family ResultDds' result frags where
  ResultDds' 'False _ = 'Nothing
  ResultDds' 'True '[] = 'Nothing
  ResultDds' 'True frags = NonEmptyK (FragsDds frags)

type ResultDds :: ∀ {ext} . Type -> [Frag ext] -> Maybe [DdK ext]
type family ResultDds clause frags where
  ResultDds clause frags = ResultDds' (ClauseResultFor clause) frags

type ResultTypes' :: ∀ {ext} . Bool -> [Frag ext] -> Maybe [Type]
type family ResultTypes' result frags where
  ResultTypes' 'False _ = 'Nothing
  ResultTypes' 'True '[] = 'Nothing
  ResultTypes' 'True frags = NonEmptyK (FragsTypes frags)

type ResultTypes :: ∀ {ext} . Type -> [Frag ext] -> Maybe [Type]
type family ResultTypes clause frags where
  ResultTypes clause frags = ResultTypes' (ClauseResultFor clause) frags
