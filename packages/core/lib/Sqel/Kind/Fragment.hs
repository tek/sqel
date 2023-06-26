module Sqel.Kind.Fragment where

import Sqel.Data.Dd (DdK)
import Sqel.Data.Fragment (Frag (Frag, FragOp), Frag0 (Frag0))
import Sqel.Data.Spine (Spine)
import Sqel.Dd (DdTypes)
import Sqel.Kind.List (type (++))
import Sqel.SOP.Error (Quoted, QuotedType)

type Frag0Tag :: Frag0 ext -> Type
type family Frag0Tag frag where
  Frag0Tag ('Frag0 tag _ _ _ _) = tag

type Frag0Dd :: Frag0 ext -> DdK ext
type family Frag0Dd frag where
  Frag0Dd ('Frag0 _ _ s _ _) = s

type Frag0Dds :: Frag0 ext -> [DdK ext]
type family Frag0Dds frag where
  Frag0Dds ('Frag0 _ _ s _ _) = '[s]

type FragDds :: Frag ext -> [DdK ext]
type family FragDds frag where
  FragDds ('Frag f) = Frag0Dds f
  FragDds ('FragOp l r) = Frag0Dds l ++ Frag0Dds r

type FragsDds :: [Frag ext] -> [DdK ext]
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
