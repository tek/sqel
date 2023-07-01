module Sqel.Data.IndexState where

import Control.Monad.Trans.State.Strict (State, evalState, state)

type IndexState a = State Int a

newIndex :: IndexState Int
newIndex = state \ i -> (i, i + 1)

withNewIndex :: (Int -> a) -> IndexState a
withNewIndex f = f <$> newIndex

indexState :: IndexState a -> a
indexState s = evalState s 1
