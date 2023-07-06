module Sqel.Build.Condition where

import Data.List.NonEmpty ((<|))
import qualified Exon as Exon
import Exon (exon)

import Sqel.CondExpr (renderCondExpr)
import Sqel.Data.Field (CondField (CondField, CondOp), CondOperand (CondOpField, CondOpLit))
import Sqel.Data.Path (PrimPath)
import qualified Sqel.Data.QueryMeta as QueryMeta
import Sqel.Data.QueryMeta (CondMeta (CondMeta), QueryMeta (QueryMeta), index)
import Sqel.Data.Selector (Selector)
import Sqel.Data.Spine (CompSort (CompCon, CompProd, CompSum), Spine (SpineNest, SpinePrim), pattern SpineComp)
import Sqel.Data.Sql (Sql, toSql)
import qualified Sqel.Default
import Sqel.Default (Def, PrimMeta (PrimMeta), SpineDef)
import Sqel.Path (primMetaPath, primSelector)
import Sqel.Sql.Prepared (dollar)

data Connective =
  And
  |
  Or
  deriving stock (Eq, Show)

data Expr =
  ExprEmpty
  |
  ExprAtom Sql
  |
  ExprConn Connective (NonEmpty Expr)
  deriving stock (Eq, Show)

pattern ExprAnd :: NonEmpty Expr -> Expr
pattern ExprAnd es = ExprConn And es

pattern ExprOr :: NonEmpty Expr -> Expr
pattern ExprOr es = ExprConn Or es

{-# complete ExprEmpty, ExprAtom, ExprAnd, ExprOr #-}

data Conds =
  Single Sql
  |
  Multi Sql

unConds :: Conds -> Sql
unConds = \case
  Single s -> s
  Multi s -> s

simplify :: Expr -> Expr
simplify = \case
  ExprEmpty -> ExprEmpty
  ExprAtom e -> ExprAtom e
  ExprConn _ [e] -> simplify e
  ExprConn c es -> flatten c es
  where
    flatten c es = ExprConn c (merge c . simplify =<< es)
    merge c = \case
      ExprConn c' es' | c == c' -> es'
      e -> pure e

concatSep :: Sql -> [Conds] -> Conds
concatSep sep =
  Multi . Exon.intercalate sep . fmap parens
  where
    parens = \case
      Single c -> c
      Multi c -> [exon|(#{c})|]

render :: Expr -> Maybe Sql
render =
  fmap unConds . spin . simplify
  where
    spin = \case
      ExprEmpty -> Nothing
      ExprAtom e -> Just (Single e)
      ExprConn c es -> multi (conn c) es
    multi sep es =
      Just (concatSep sep (mapMaybe spin (toList es)))
    conn = \case
      And -> " and "
      Or -> " or "

nullGuard :: Maybe Bool -> Int -> Sql -> Sql
nullGuard (Just True) index code = [exon|(#{dollar index} is null or #{code})|]
nullGuard _ _ code = code

conGuard :: Int -> Int -> Expr -> Expr
conGuard indexParam conIndex = \case
  ExprEmpty -> ExprEmpty
  e -> ExprAnd [grd, e]
  where
    grd = ExprAtom [exon|#{dollar indexParam} = #{show conIndex}|]

conditionPath :: Bool -> PrimMeta -> Selector
conditionPath multi meta =
  primSelector (primMetaPath multi meta)

paramCondition :: PrimPath -> Int -> CondMeta -> Expr
paramCondition path index (CondMeta code nullable) =
  ExprAtom (nullGuard nullable index cond)
  where
    cond = case code of
      QueryMeta.CondOp op -> [exon|##{primSelector path} #{op} #{dollar index}|]
      QueryMeta.CondExpr expr -> renderCondExpr path index expr

prim :: Bool -> PrimMeta -> Expr
prim multi meta
  | QueryMeta index (Just cond) <- meta.query = paramCondition (primMetaPath multi meta) index cond
  | otherwise = ExprEmpty

joinConds :: (NonEmpty Expr -> Expr) -> [Expr] -> Expr
joinConds sep =
  maybe ExprEmpty sep . foldr folder Nothing
  where
    folder ExprEmpty z = z
    folder e Nothing = Just (pure e)
    folder e (Just z) = Just (e <| z)

joinComp :: CompSort Def -> [Expr] -> Expr
joinComp compSort sub =
  joinConds (op compSort) sub
  where
    op = \case
      CompSum _ -> ExprOr
      CompProd -> ExprAnd
      CompCon -> ExprAnd

comp :: Bool -> CompSort Def -> [SpineDef] -> Expr
comp multi compSort sub
  | CompSum PrimMeta {query = QueryMeta {index}} <- compSort =
    joinComp compSort (uncurry (conGuard index) <$> zip [0..] (node multi <$> sub))
  | otherwise =
    joinComp compSort (node multi <$> sub)

node :: Bool -> SpineDef -> Expr
node multi = \case
  SpinePrim meta -> prim multi meta
  SpineComp _ compSort sub -> comp multi compSort sub

spineConditions :: Bool -> SpineDef -> Expr
spineConditions multi = \case
  SpineNest _ compSort sub -> comp multi compSort sub
  n -> node multi n

-- TODO support ops on comps
condOperand ::
  Bool ->
  CondOperand Def ->
  Maybe Sql
condOperand multi = \case
  CondOpLit s -> Just s
  CondOpField (SpinePrim meta) -> Just (toSql (conditionPath multi meta))
  CondOpField _ -> Nothing

fieldOpCondition :: Bool -> Text -> CondOperand Def -> CondOperand Def -> Expr
fieldOpCondition multi op left right =
  case (condOperand multi left, condOperand multi right) of
    (Just l, Just r) ->
      ExprAtom [exon|#{l} ##{op} #{r}|]
    _ ->
      ExprEmpty

fieldConditions :: Bool -> CondField Def -> Expr
fieldConditions multi = \case
  CondField s -> spineConditions multi s
  CondOp op l r ->
    fieldOpCondition multi op l r

conditionsClause :: Bool -> [CondField Def] -> Maybe Sql
conditionsClause multi fields =
  render (joinConds ExprAnd (fieldConditions multi <$> fields))
