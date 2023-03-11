module Sqel.Query.SelectExpr where

import Generics.SOP (All, I (I), K (K), NP (Nil, (:*)), hcmap, hcollapse)

import Sqel.Data.Dd (
  Comp (Prod, Sum),
  CompInc (Merge, Nest),
  Dd (Dd),
  DdInc (DdMerge, DdNest),
  DdK (DdK),
  DdSort (DdSum),
  DdStruct (DdComp, DdPrim),
  QOp (QAnd),
  Struct (Comp, Prim),
  )
import Sqel.Data.FragType (FragType (Where))
import Sqel.Data.Mods (Ignore (Ignore), Mods (Mods), Nullable (Nullable), Newtype)
import Sqel.Data.Sel (Sel (SelSymbol, SelUnused), SelW (SelWAuto, SelWSymbol))
import Sqel.Data.SelectExpr (
  SelectAtom (SelectAtom),
  SelectExpr (SelectExprAtom, SelectExprIgnore, SelectExprList, SelectExprSum),
  )
import Sqel.Data.Selector (Selector (Selector))
import Sqel.Data.Sql (Sql (Sql), sql)
import Sqel.Prim (IndexColumn)
import Sqel.Query.Combinators (whereEq)
import Sqel.Query.Fragments (ColumnPrefix, QFragmentPrefix (qfragmentPrefix), prefixed)
import Sqel.Sql.Prepared (dollar)
import Sqel.Text.DbIdentifier (dbSymbol)

guardSum :: SelectExpr -> SelectExpr
guardSum = \case
  SelectExprAtom Where code -> SelectExprAtom Where \ i -> [sql|(#{dollar i} is null or #{code i})|]
  SelectExprList op sub -> SelectExprList op (guardSum <$> sub)
  expr -> expr

skipPrimMod ::
  PrimSelectExpr mods =>
  Maybe Selector ->
  Maybe SelectAtom ->
  Mods (mod : mods) ->
  SelectExpr
skipPrimMod selector prev (Mods (_ :* mods)) =
  primSelectExpr selector prev (Mods mods)

type PrimSelectExpr :: [Type] -> Constraint
class PrimSelectExpr mods where
  primSelectExpr :: Maybe Selector -> Maybe SelectAtom -> Mods mods -> SelectExpr

instance PrimSelectExpr '[] where
  primSelectExpr selector prev (Mods Nil) =
    SelectExprAtom type_ (code (fold selector))
    where
      SelectAtom type_ code =
        fromMaybe whereEq prev

instance (
    PrimSelectExpr mods
  ) => PrimSelectExpr (SelectAtom : mods) where
    primSelectExpr selector _ (Mods (I atom :* mods)) =
      primSelectExpr selector (Just atom) (Mods mods)

instance PrimSelectExpr (Ignore : mods) where
    primSelectExpr _ _ (Mods (I Ignore :* _)) =
      SelectExprIgnore

instance (
    PrimSelectExpr mods
  ) => PrimSelectExpr (Nullable : mods) where
    primSelectExpr selector prev (Mods (I Nullable :* mods)) =
      case primSelectExpr selector prev (Mods mods) of
        SelectExprAtom type_ code ->
          SelectExprAtom type_ \ i -> [sql|(#{dollar i} is null or #{code i})|]
        expr -> expr

instance (
    PrimSelectExpr mods
  ) => PrimSelectExpr (Newtype a w : mods) where
    primSelectExpr = skipPrimMod

type ToSelectExpr :: DdK -> Constraint
class ToSelectExpr query where
  toSelectExpr :: ColumnPrefix -> Dd query -> SelectExpr

instance (
    PrimSelectExpr mods
  ) => ToSelectExpr ('DdK ('SelSymbol n) mods q 'Prim) where
    toSelectExpr pre (Dd (SelWSymbol Proxy) mods DdPrim) =
      primSelectExpr (Just selector) Nothing mods
      where
        selector = Selector (Sql (prefixed (dbSymbol @n) pre))

instance (
    PrimSelectExpr mods
  ) => ToSelectExpr ('DdK 'SelUnused mods q 'Prim) where
    toSelectExpr _ (Dd _ mods DdPrim) =
      primSelectExpr Nothing Nothing mods

prodSelectExpr ::
  ∀ sel s .
  All ToSelectExpr s =>
  QFragmentPrefix sel =>
  SelW sel ->
  ColumnPrefix ->
  QOp ->
  NP Dd s ->
  SelectExpr
prodSelectExpr sel pre op =
  SelectExprList op . hcollapse . hcmap (Proxy @ToSelectExpr) (K . toSelectExpr (qfragmentPrefix sel pre))

sumSelectExpr ::
  ∀ sel s .
  All ToSelectExpr s =>
  QFragmentPrefix sel =>
  SelW sel ->
  ColumnPrefix ->
  NP Dd s ->
  SelectExpr
sumSelectExpr sel pre =
  SelectExprSum . hcollapse . hcmap (Proxy @ToSelectExpr) (K . toSelectExpr (qfragmentPrefix sel pre))

-- TODO add QOp param lookup
instance (
    All ToSelectExpr sub,
    QFragmentPrefix sel
  ) => ToSelectExpr ('DdK sel mods q ('Comp tsel ('Prod con) 'Nest sub)) where
  toSelectExpr pre = \case
    Dd sel _ (DdComp _ _ DdNest sub) ->
      prodSelectExpr sel pre QAnd sub

instance (
    All ToSelectExpr sub
  ) => ToSelectExpr ('DdK sel mods q ('Comp tsel ('Prod con) 'Merge sub)) where
  toSelectExpr pre = \case
    Dd _ _ (DdComp _ _ DdMerge sub) ->
      prodSelectExpr SelWAuto pre QAnd sub

instance (
    All ToSelectExpr sub,
    QFragmentPrefix sel
  ) => ToSelectExpr ('DdK sel mods q ('Comp tsel 'Sum 'Nest (IndexColumn name : sub))) where
  toSelectExpr pre = \case
    Dd sel _ (DdComp _ DdSum DdNest (_ :* sub)) ->
      guardSum (sumSelectExpr sel pre sub)
