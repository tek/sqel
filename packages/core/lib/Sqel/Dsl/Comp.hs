module Sqel.Dsl.Comp where

type Gen :: Type
data Gen

type Prod :: [Type] -> Type
data Prod cols

type ProdAs :: Symbol -> [Type] -> Type
data ProdAs tname cols

type UidProd :: Type -> Type -> Type
data UidProd i a

type Con :: [Type] -> Type
data Con cols

type Con1 :: Type -> Type
data Con1 cols

type Sum :: [Type] -> Type
data Sum cons

type Merge :: Type -> Type
data Merge spec
