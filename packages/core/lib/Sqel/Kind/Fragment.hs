module Sqel.Kind.Fragment where

import Sqel.Data.Dd (Dd)
import Sqel.Data.Fragment (Frag (Frag, FragOp), Frag0 (Frag0), FragOperand (FragOpFrag, FragOpLit))
import Sqel.Data.Spine (Spine)
import Sqel.Dd (DdTypes)
import Sqel.Kind.List (type (++))
import Sqel.Kind.Error (Quoted, QuotedType)

type Frag0Tag :: Frag0 ext -> Type
type family Frag0Tag frag where
  Frag0Tag ('Frag0 tag _ _ _ _) = tag

type Frag0Dd :: Frag0 ext -> Dd ext
type family Frag0Dd frag where
  Frag0Dd ('Frag0 _ _ s _ _) = s

type Frag0Dds :: Frag0 ext -> [Dd ext]
type family Frag0Dds frag where
  Frag0Dds ('Frag0 _ _ s _ _) = '[s]

type FragOperandDds :: FragOperand ext -> [Dd ext]
type family FragOperandDds frag where
  FragOperandDds ('FragOpLit _) = '[]
  FragOperandDds ('FragOpFrag frag) = Frag0Dds frag

type FragDds :: Frag ext -> [Dd ext]
type family FragDds frag where
  FragDds ('Frag f) = Frag0Dds f
  FragDds ('FragOp l r) = FragOperandDds l ++ FragOperandDds r

type FragsDds :: [Frag ext] -> [Dd ext]
type family FragsDds frags where
  FragsDds '[] = '[]
  FragsDds (f : fs) = FragDds f ++ FragsDds fs

type FragsTypes :: [Frag ext] -> [Type]
type family FragsTypes frags where
  FragsTypes fs = DdTypes (FragsDds fs)

type FragDd :: Frag ext -> dd
type family FragDd frag where
  FragDd ('Frag ('Frag0 _ _ s _ _)) = s
  FragDd ('FragOp _ _) =
    TypeError ("Internal: " <> Quoted "FragDd" <> " called with " <> Quoted "FragOp" <> " or " <> QuotedType Spine)
