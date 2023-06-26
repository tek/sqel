module Sqel.Fragment where

import Sqel.Data.Fragment (Frag (Frag, FragOp), Fragment (Fragment, FragmentOp))
import Sqel.Data.Sqel (SqelFor)
import Sqel.Kind.Fragment (Frag0Dd, Frag0Tag)

(.=) ::
  Fragment ('Frag l) ->
  Fragment ('Frag r) ->
  Fragment ('FragOp l r)
(.=) l r =
  FragmentOp "=" l r

getFragment ::
  ∀ frag .
  Fragment ('Frag frag) ->
  SqelFor (Frag0Tag frag) (Frag0Dd frag)
getFragment (Fragment s) = s
