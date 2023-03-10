module Sqel.SOP.Error where

import Fcf (Eval, Exp, UnList, type (@@))
import Type.Errors (ErrorMessage (Text))

type family JoinSep (sep :: Symbol) (ss :: [Symbol]) :: Symbol where
  JoinSep _ '[] = "<empty>"
  JoinSep _ '[s] = s
  JoinSep sep (s : ss) = AppendSymbol (AppendSymbol s sep) (JoinSep sep ss)

type family JoinError (sep :: ErrorMessage) (ns :: [ErrorMessage]) :: ErrorMessage where
  JoinError _ '[] = 'Text "<empty>"
  JoinError sep (n : n1 : ns) = n <> sep <> JoinError sep (n1 : ns)
  JoinError _ '[n] = n

type family JoinComma (ns :: [ErrorMessage]) :: ErrorMessage where
  JoinComma ns = JoinError ('Text ", ") ns

type family JoinSym (sep :: Symbol) (ns :: [Symbol]) :: ErrorMessage where
  JoinSym sep (n : n1 : ns) = n <> sep <> JoinSym sep (n1 : ns)
  JoinSym _ '[n] = 'Text n

type family JoinCommaSym (ns :: [Symbol]) :: ErrorMessage where
  JoinCommaSym (n : n1 : ns) = 'Text n <> ", " <> JoinCommaSym (n1 : ns)
  JoinCommaSym '[n] = 'Text n

type family QuotedError (msg :: ErrorMessage) :: ErrorMessage where
  QuotedError err = "‘" <> err <> "’"

type family Quoted (s :: Symbol) :: ErrorMessage where
  Quoted s = QuotedError ('Text s)

type family QuotedType (t :: k) :: ErrorMessage where
  QuotedType t = QuotedError ('ShowType t)

data LineBreak :: ErrorMessage -> ErrorMessage -> Exp ErrorMessage
type instance Eval (LineBreak l r) = l % r

type family Unlines (fragments :: [ErrorMessage]) :: ErrorMessage where
  Unlines '[] =
    'Text ""
  Unlines (h : t) =
    UnList h LineBreak @@ t
