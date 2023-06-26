module Sqel.SOP.Error where

import Fcf (Eval, Exp, type (@@))
import Fcf.Class.Functor (FMap)
import Fcf.Data.List (Reverse)
import Type.Errors (DelayError, DelayErrorFcf, ErrorMessage (Text), IfStuck)

type JoinSep :: Symbol -> [Symbol] -> Symbol
type family JoinSep sep ss where
  JoinSep _ '[] = "<empty>"
  JoinSep _ '[s] = s
  JoinSep sep (s : ss) = AppendSymbol (AppendSymbol s sep) (JoinSep sep ss)

type Unwords :: [Symbol] -> Symbol
type family Unwords syms where
  Unwords syms = JoinSep " " syms

type JoinError :: ErrorMessage -> [ErrorMessage] -> ErrorMessage
type family JoinError sep ns where
  JoinError _ '[] = 'Text "<empty>"
  JoinError sep (n : n1 : ns) = n <> sep <> JoinError sep (n1 : ns)
  JoinError _ '[n] = n

type JoinComma :: [ErrorMessage] -> ErrorMessage
type family JoinComma ns where
  JoinComma ns = JoinError ('Text ", ") ns

type JoinSym :: Symbol -> [Symbol] -> ErrorMessage
type family JoinSym sep ns where
  JoinSym sep (n : n1 : ns) = n <> sep <> JoinSym sep (n1 : ns)
  JoinSym _ '[n] = 'Text n

type JoinCommaSym :: [Symbol] -> ErrorMessage
type family JoinCommaSym ns where
  JoinCommaSym (n : n1 : ns) = 'Text n <> ", " <> JoinCommaSym (n1 : ns)
  JoinCommaSym '[n] = 'Text n

type QuotedError :: ErrorMessage -> ErrorMessage
type family QuotedError msg where
  QuotedError err = "‘" <> err <> "’"

type Quoted :: k -> ErrorMessage
type family Quoted s where
  Quoted s = QuotedError (ToErrorMessage s)

type QuotedType :: k -> ErrorMessage
type family QuotedType t where
  QuotedType t = QuotedError ('ShowType t)

data LineBreak :: ErrorMessage -> ErrorMessage -> Exp ErrorMessage
type instance Eval (LineBreak l r) = l % r

type Unlines :: [ErrorMessage] -> ErrorMessage
type family Unlines lines where
  Unlines '[] = 'Text ""
  Unlines '[x] = x
  Unlines (h : t) = h % Unlines t

type UnlinesWith :: (k -> Exp ErrorMessage) -> [k] -> ErrorMessage
type family UnlinesWith f xs where
  UnlinesWith _ '[] = 'Text ""
  UnlinesWith f xs = Unlines (FMap f @@ xs)

type ShowPath :: [Symbol] -> ErrorMessage
type family ShowPath path where
  ShowPath path = Quoted (JoinSep "." (Reverse @@ path))

data ToError :: Symbol -> Exp ErrorMessage
type instance Eval (ToError s) = 'Text s

type UnlinesSym ss = UnlinesWith ToError ss

data Bulleted :: k -> Exp ErrorMessage
type instance Eval (Bulleted s) = "• " <> s

type BulletedLines s = UnlinesWith Bulleted s

type StuckError :: k0 -> ErrorMessage -> ErrorMessage -> k
type family StuckError thing stuck unstuck where
  StuckError thing stuck unstuck =
    IfStuck thing
    (DelayError stuck)
    (DelayErrorFcf unstuck)
