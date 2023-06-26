module Sqel.Data.Class.Dd where

import Sqel.Data.Dd (Inc (Merge, Nest), SInc (SMerge, SNest))

class SingInc i where
  singInc :: SInc i

instance SingInc 'Nest where
  singInc = SNest

instance SingInc 'Merge where
  singInc = SMerge
