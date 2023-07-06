module Sqel.Fragment where

import Sqel.Data.Fragment (
  Frag (Frag, FragLit, FragOp),
  FragOperand (FragOpFrag, FragOpLit),
  Fragment (FragmentLit, FragmentOp),
  FragmentOperand (FragmentOpFrag, FragmentOpLit),
  )
import Sqel.Kind.Error (PlainTypeError)

data Operands =
  BothFrag
  |
  LeftFrag
  |
  RightFrag

-- type family LitMismatch (frag :: Frag0 ext) (b :: Type) :: k where
--   LitMismatch ('Frag0 _ _ ('Dd _ a _) _ _) b =
--     TypeError ("The literal type " <> Quoted b <> " does not match the field's type " <> Quoted a)

type NestedOp :: k
type family NestedOp where
  NestedOp = PlainTypeError "Nested operations are not supported."

type Triage :: Type -> Type -> Operands
type family Triage l r where
  Triage (Fragment ('FragOp _ _)) _ =
    NestedOp
  Triage _ (Fragment ('FragOp _ _)) =
    NestedOp
  Triage (Fragment _) (Fragment _) =
    'BothFrag
  Triage (Fragment _) _ =
    'LeftFrag
  Triage _ (Fragment _) =
    'RightFrag
  Triage _ _ =
    PlainTypeError "Invalid operand type(s): At least one must be a table or query field."

type FragmentOperation :: ∀ {ext} . Operands -> Type -> Type -> FragOperand ext -> FragOperand ext -> Constraint
class FragmentOperation ops l r fl fr | ops l r -> fl fr where
  fragmentOperation :: Text -> l -> r -> Fragment ('FragOp fl fr)

instance FragmentOperation 'LeftFrag (Fragment ('Frag l)) a ('FragOpFrag l) ('FragOpLit a) where
  fragmentOperation op l r =
    FragmentOp op (FragmentOpFrag l) (FragmentOpLit r)

instance FragmentOperation 'RightFrag a (Fragment ('Frag l)) ('FragOpLit a) ('FragOpFrag l) where
  fragmentOperation op l r =
    FragmentOp op (FragmentOpLit l) (FragmentOpFrag r)

instance FragmentOperation 'BothFrag (Fragment ('Frag l)) (Fragment ('Frag r)) ('FragOpFrag l) ('FragOpFrag r) where
  fragmentOperation op l r =
    FragmentOp op (FragmentOpFrag l) (FragmentOpFrag r)

type Op =
  ∀ {ext} l r (fl :: FragOperand ext) (fr :: FragOperand ext) .
  FragmentOperation (Triage l r) l r fl fr =>
  l ->
  r ->
  Fragment ('FragOp fl fr)

checkOp ::
  ∀ l r fl fr .
  FragmentOperation (Triage l r) l r fl fr =>
  Text ->
  l ->
  r ->
  Fragment ('FragOp fl fr)
checkOp =
  fragmentOperation @(Triage l r)

(.=) :: Op
(.=) l r = checkOp "=" l r

(.!=) :: Op
(.!=) l r = checkOp "!=" l r

(.<) :: Op
(.<) l r = checkOp "<" l r

(.<=) :: Op
(.<=) l r = checkOp "<=" l r

(.>) :: Op
(.>) l r = checkOp ">" l r

(.>=) :: Op
(.>=) l r = checkOp ">=" l r

lit :: a -> Fragment ('FragLit a)
lit = FragmentLit
