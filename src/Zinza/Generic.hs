{-# LANGUAGE TypeOperators, FlexibleContexts, ScopedTypeVariables, TypeFamilies #-}
module Zinza.Generic (
    Zinza (..),
    stripFieldPrefix,
    GZinzaType, genericToType,
    GZinzaValue, genericToValue,
    ) where

import Data.Proxy (Proxy (..))
import GHC.Generics
import Data.Kind (Type)
import Data.Char (isLower, toLower)
import Data.List (stripPrefix)

import qualified Data.Map.Strict as M

import Zinza.Value
import Zinza.Var (Var)
import Zinza.Type
import Zinza.Class

-------------------------------------------------------------------------------
-- Field renamer
-------------------------------------------------------------------------------

stripFieldPrefix
    :: forall a. (Generic a, GFieldNames (Rep a))
    => Proxy a
    -> String -> String
stripFieldPrefix _ = case fieldNames (Proxy :: Proxy (Rep a)) of
    []            -> id
    (fieldname:_) -> \fn -> case stripPrefix pfx fn of
        Just (x:xs) -> toLower x : xs
        _           -> fn -- otherwise don't hcange
      where
        (pfx, _) = span isLower fieldname

class GFieldNames (f :: Type -> Type) where
    fieldNames :: Proxy f -> [String]

instance (i ~ D, GFieldNamesSum f) => GFieldNames (M1 i c f) where
    fieldNames _ = fieldNamesSum (Proxy :: Proxy f)

class GFieldNamesSum (f :: Type -> Type) where
    fieldNamesSum :: Proxy f -> [String]

instance (i ~ C, GFieldNamesProd f) => GFieldNamesSum (M1 i c f ) where
    fieldNamesSum _ = fieldNamesProd (Proxy :: Proxy f)
    
class GFieldNamesProd (f :: Type -> Type) where
    fieldNamesProd :: Proxy f -> [String]

instance (GFieldNamesProd f, GFieldNamesProd g) => GFieldNamesProd (f :*: g) where
    fieldNamesProd _ = fieldNamesProd (Proxy :: Proxy f) ++ fieldNamesProd (Proxy :: Proxy g)

instance (i ~ S, Selector c) => GFieldNamesProd (M1 i c f) where
    fieldNamesProd _ = [selName (undefined :: M1 i c f ())]

-------------------------------------------------------------------------------
-- Generic toType
-------------------------------------------------------------------------------

genericToType
    :: forall a. (Generic a, GZinzaType (Rep a)) 
    => (String -> String)  -- ^ field renamer
    -> Proxy a -> Ty
genericToType namer _ = TyRecord $ M.fromList
    [ (namer fn, (fn, ty))
    | (fn, ty) <- gtoType (Proxy :: Proxy (Rep a))
    ]

class GZinzaType (f :: Type -> Type) where
    gtoType :: Proxy f -> [(String, Ty)]

instance (i ~ D, GZinzaTypeSum f) => GZinzaType (M1 i c f) where
    gtoType _ = gtoTypeSum (Proxy :: Proxy f)

class GZinzaTypeSum (f :: Type -> Type) where
    gtoTypeSum :: Proxy f -> [(String, Ty)]

instance (i ~ C, GZinzaTypeProd f) => GZinzaTypeSum (M1 i c f ) where
    gtoTypeSum _ = gtoTypeProd (Proxy :: Proxy f)
    
class GZinzaTypeProd (f :: Type -> Type) where
    gtoTypeProd :: Proxy f -> [(String, Ty)]

instance (GZinzaTypeProd f, GZinzaTypeProd g) => GZinzaTypeProd (f :*: g) where
    gtoTypeProd _ = gtoTypeProd (Proxy :: Proxy f) ++ gtoTypeProd (Proxy :: Proxy g)

instance (i ~ S, Selector c, GZinzaTypeLeaf f) => GZinzaTypeProd (M1 i c f) where
    gtoTypeProd _ = [(selName (undefined :: M1 i c f ()), gtoTypeLeaf (Proxy :: Proxy f))]

class GZinzaTypeLeaf (f :: Type -> Type) where
    gtoTypeLeaf :: Proxy f -> Ty
    
instance (i ~ R, Zinza a) => GZinzaTypeLeaf (K1 i a) where
    gtoTypeLeaf _ = toType (Proxy :: Proxy a)

-------------------------------------------------------------------------------
-- Generic toValue
-------------------------------------------------------------------------------

genericToValue
    :: forall a. (Generic a, GZinzaValue (Rep a)) 
    => (String -> String)  -- ^ field renamer
    -> a -> Value
genericToValue namer x = VRecord $ M.fromList
    [ (namer fn, e)
    | (fn, e) <- gtoValue (from x)
    ]

class GZinzaValue (f :: Type -> Type) where
    gtoValue :: f () -> [(Var, Value)]

instance (i ~ D, GZinzaValueSum f) => GZinzaValue (M1 i c f) where
    gtoValue = gtoValueSum .  unM1

class GZinzaValueSum (f :: Type -> Type) where
    gtoValueSum :: f () -> [(Var, Value)]

instance (i ~ C, GZinzaValueProd f) => GZinzaValueSum (M1 i c f) where
    gtoValueSum = gtoValueProd . unM1

class GZinzaValueProd (f :: Type -> Type) where
    gtoValueProd :: f () -> [(Var, Value)]

instance (GZinzaValueProd f, GZinzaValueProd g) => GZinzaValueProd (f :*: g) where
    gtoValueProd (f :*: g) = gtoValueProd f ++ gtoValueProd g

instance (i ~ S, Selector c, GZinzaValueLeaf f) => GZinzaValueProd (M1 i c f) where
    gtoValueProd (M1 x) = [(selName (undefined :: M1 i c f ()), gtoValueLeaf x)]

class GZinzaValueLeaf f where
    gtoValueLeaf :: f a -> Value

instance (i ~ R, Zinza a) => GZinzaValueLeaf (K1 i a) where
    gtoValueLeaf (K1 a) = toValue a
