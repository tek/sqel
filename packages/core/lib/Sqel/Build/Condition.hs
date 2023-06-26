module Sqel.Build.Condition where

import Data.List.NonEmpty ((<|))
import Exon (exon)
import qualified Exon.Combinators as Exon

import Sqel.Data.Field (CondField (CondField, CondOp))
import Sqel.Data.PgTypeName (PgTableName)
import Sqel.Data.Selector (Selector, pathSelectorTable)
import Sqel.Data.Spine (
  CompSort (CompCon, CompProd, CompSum),
  Spine (SpineNest, SpinePrim),
  pattern SpineComp,
  SpinePath (SpinePath),
  )
import Sqel.Data.Sql (Sql)
import qualified Sqel.Default
import Sqel.Default (CondMeta (CondMeta), Def, PrimMeta (PrimMeta), QueryMeta (QueryMeta), SpineDef)
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

selector :: Maybe PgTableName -> SpinePath -> Selector
selector table (SpinePath path) =
  pathSelectorTable table path

nullGuard :: Maybe Bool -> Int -> Sql -> Sql
nullGuard (Just True) index code = [exon|(#{dollar index} is null or #{code})|]
nullGuard _ _ code = code

conGuard :: Int -> Int -> Expr -> Expr
conGuard indexParam conIndex = \case
  ExprEmpty -> ExprEmpty
  e -> ExprAnd [grd, e]
  where
    grd = ExprAtom [exon|#{dollar indexParam} = #{show conIndex}|]

conditionPath :: Bool -> SpinePath -> PgTableName -> Selector
conditionPath multi path table =
  selector selTable path
  where
    selTable = if multi then Just table else Nothing

paramCondition :: Bool -> SpinePath -> PgTableName -> Int -> CondMeta -> Expr
paramCondition multi path table index (CondMeta op nullable) =
  ExprAtom (nullGuard nullable index cond)
  where
    cond = [exon|##{conditionPath multi path table} #{op} #{dollar index}|]

prim :: Bool -> PrimMeta -> Expr
prim multi meta
  | QueryMeta index (Just cond) <- meta.query = paramCondition multi meta.path meta.table index cond
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

fieldOpCondition :: Bool -> Text -> SpineDef -> SpineDef -> Expr
fieldOpCondition multi op (SpinePrim lmeta) (SpinePrim rmeta) =
  ExprAtom [exon|##{conditionPath multi lmeta.path lmeta.table} ##{op} ##{conditionPath multi rmeta.path rmeta.table}|]
fieldOpCondition _ _ _ _ =
  ExprEmpty

fieldConditions :: Bool -> CondField Def -> Expr
fieldConditions multi = \case
  CondField s -> spineConditions multi s
  CondOp op l r ->
    fieldOpCondition multi op l r

conditionsClause :: Bool -> [CondField Def] -> Maybe Sql
conditionsClause multi fields =
  render (joinConds ExprAnd (fieldConditions multi <$> fields))
