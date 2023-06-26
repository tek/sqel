module Sqel.SOP.HasGeneric where

import Generics.SOP.GGP (GCode, GDatatypeInfoOf)
import Generics.SOP.Type.Metadata (DatatypeInfo (Newtype))

type BoolVal :: Bool -> Constraint
class BoolVal flag where
  boolVal :: Bool

instance BoolVal 'True where
  boolVal = True

instance BoolVal 'False where
  boolVal = False

justBoolVal :: ∀ (flag :: Bool) a . BoolVal flag => a -> Maybe a
justBoolVal a | boolVal @flag = Just a
              | otherwise = Nothing

class GDatatypeInfoIsNewtype (dss :: [[Type]]) (info :: DatatypeInfo) (wrapped :: Maybe Type) | dss info -> wrapped
instance {-# incoherent #-} wrapped ~ 'Nothing => GDatatypeInfoIsNewtype dss info wrapped
instance wrapped ~ 'Just d => GDatatypeInfoIsNewtype '[ '[d]] ('Newtype m n c) wrapped

class IsNewtype d (wrapped :: Maybe Type) | d -> wrapped
instance GDatatypeInfoIsNewtype (GCode d) (GDatatypeInfoOf d) wrapped => IsNewtype d wrapped

class GCodeResolves (d :: [[Type]]) (flag :: Bool) | d -> flag
instance {-# incoherent #-} flag ~ 'False => GCodeResolves d flag
instance flag ~ 'True => GCodeResolves (d : ds) flag
instance flag ~ 'True => GCodeResolves '[] flag

class HasGeneric d (flag :: Bool) | d -> flag
instance GCodeResolves (GCode d) flag => HasGeneric d flag

class GCodeResolvesNot (d :: [[Type]]) (flag :: Bool) | d -> flag where
  gcodeResolvesNot :: Bool
instance {-# incoherent #-} flag ~ 'True => GCodeResolvesNot d flag where
  gcodeResolvesNot =
    True
instance flag ~ 'False => GCodeResolvesNot (d : ds) flag where
  gcodeResolvesNot =
    False

class HasNoGeneric d (flag :: Bool) | d -> flag where
  hasNoGeneric :: Bool

instance (BoolVal flag, GCodeResolvesNot (GCode d) flag) => HasNoGeneric d flag where
  hasNoGeneric = boolVal @flag
