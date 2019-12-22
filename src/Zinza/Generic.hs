{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Zinza.Generic (
    Zinza (..),
    GFieldNames, stripFieldPrefix,
    GZinzaType, genericToType, genericToTypeSFP,
    GZinzaValue, genericToValue, genericToValueSFP,
    GZinzaFrom, genericFromValue, genericFromValueSFP,
    ) where

import Data.Char      (isLower, toLower)
import Data.Kind      (Type)
import Data.List      (stripPrefix)
import Data.Proxy     (Proxy (..))
import Data.Semigroup (Semigroup (..))
import GHC.Generics

import qualified Data.Map.Strict as M

import Zinza.Class
import Zinza.Errors
import Zinza.Pos
import Zinza.Type
import Zinza.Value
import Zinza.Var    (Var)

-- $setup
-- >>> :set -XDeriveGeneric
-- >>> import Data.Proxy (Proxy (..))

-------------------------------------------------------------------------------
-- Field renamer
-------------------------------------------------------------------------------

-- | Field renamer which will automatically strip lowercase prefix from
-- field names.
--
-- >>> data R = R { recFoo :: Int, recBar :: Char } deriving Generic
-- >>> stripFieldPrefix (Proxy :: Proxy R) "recFoo"
-- "foo"
--
-- If whole field is lower case, it's left intact
--
-- >>> newtype Wrapped = Wrap { unwrap :: String } deriving Generic
-- >>> stripFieldPrefix (Proxy :: Proxy Wrapped) "unwrap"
-- "unwrap"
--
stripFieldPrefix
    :: forall a. (Generic a, GFieldNames (Rep a))
    => Proxy a
    -> String -> String
stripFieldPrefix _ = case fieldNames (Proxy :: Proxy (Rep a)) of
    []     -> id
    (y:ys) -> \fn -> case stripPrefix pfx fn of
        Just (x:xs) -> toLower x : xs
        _           -> fn -- otherwise don't hcange
      where
        (pfx, _) = span isLower $ getCommonPrefix $ foldl (\cp z -> cp <> CP z) (CP y) ys

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
-- Common prefix
-------------------------------------------------------------------------------

newtype CommonPrefix = CP { getCommonPrefix :: String }

instance Data.Semigroup.Semigroup CommonPrefix where
    CP a <> CP b = CP (commonPrefix a b)

commonPrefix :: Eq a => [a] -> [a] -> [a]
commonPrefix xs@[]  _      = xs
commonPrefix _      ys@[]  = ys
commonPrefix (x:xs) (y:ys)
    | x == y    = x : commonPrefix xs ys
    | otherwise = []

-------------------------------------------------------------------------------
-- Generic toType
-------------------------------------------------------------------------------

-- | Generically derive 'toType' function.
genericToType
    :: forall a. (Generic a, GZinzaType (Rep a))
    => (String -> String)  -- ^ field renamer
    -> Proxy a -> Ty
genericToType namer _ = TyRecord $ M.fromList
    [ (namer fn, (fn, ty))
    | (fn, ty) <- gtoType (Proxy :: Proxy (Rep a))
    ]

-- | 'genericToType' with 'stripFieldPrefix'.
genericToTypeSFP
    :: forall a. (Generic a, GZinzaType (Rep a), GFieldNames (Rep a))
    => Proxy a -> Ty
genericToTypeSFP p = genericToType (stripFieldPrefix p) p

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

-- | Generically derive 'toValue' function.
genericToValue
    :: forall a. (Generic a, GZinzaValue (Rep a))
    => (String -> String)  -- ^ field renamer
    -> a -> Value
genericToValue namer x = VRecord $ M.fromList
    [ (namer fn, e)
    | (fn, e) <- gtoValue (from x)
    ]

-- | 'genericToValue' with 'stripFieldPrefix'.
genericToValueSFP
    :: forall a. (Generic a, GZinzaValue (Rep a), GFieldNames (Rep a))
    => a -> Value
genericToValueSFP = genericToValue (stripFieldPrefix (Proxy :: Proxy a))

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

-------------------------------------------------------------------------------
-- Generic fromValue
-------------------------------------------------------------------------------

genericFromValue
    :: forall a. (Generic a, GZinzaFrom (Rep a))
    => (String -> String) -- ^ field renamer
    -> Loc -> Value -> Either RuntimeError a
genericFromValue namer l v@(VRecord m) = do
    g <- gfromValue l (valueType v) $ \n -> M.lookup (namer n) m
    return (to g)
genericFromValue _ l v = throwRuntime $ NotRecord l (valueType v)

-- | 'genericFromValue' with 'stripFieldPrefix'.
genericFromValueSFP
    :: forall a. (Generic a, GZinzaFrom (Rep a), GFieldNames (Rep a))
    => Loc -> Value -> Either RuntimeError a
genericFromValueSFP = genericFromValue (stripFieldPrefix (Proxy :: Proxy a))

class GZinzaFrom (f :: Type -> Type) where
    gfromValue :: Loc -> Ty -> (Var -> Maybe Value) -> Either RuntimeError (f ())

instance (i ~ D, GZinzaFromSum f) => GZinzaFrom (M1 i c f) where
    gfromValue l ty = fmap M1 . gfromValueSum l ty

class GZinzaFromSum (f :: Type -> Type) where
    gfromValueSum :: Loc -> Ty -> (Var -> Maybe Value) -> Either RuntimeError (f ())

instance (i ~ C, GZinzaFromProd f) => GZinzaFromSum (M1 i c f) where
    gfromValueSum l ty = fmap M1 . gfromValueProd l ty

class GZinzaFromProd (f :: Type -> Type) where
    gfromValueProd ::  Loc -> Ty -> (Var -> Maybe Value) -> Either RuntimeError (f ())

instance (GZinzaFromProd f, GZinzaFromProd g) => GZinzaFromProd (f :*: g) where
    gfromValueProd l ty v = (:*:)
        <$> gfromValueProd l ty v
        <*> gfromValueProd l ty v

instance (i ~ S, Selector c, GZinzaFromLeaf f) => GZinzaFromProd (M1 i c f) where
    gfromValueProd l ty f = case f n of
        Nothing -> throwRuntime $ FieldNotInRecord l n ty
        Just v  -> M1 <$> gfromValueLeaf l v
      where
        n = selName (undefined :: M1 i c f ())

class GZinzaFromLeaf f where
    gfromValueLeaf :: Loc -> Value -> Either RuntimeError (f ())

instance (i ~ R, Zinza a) => GZinzaFromLeaf (K1 i a) where
    gfromValueLeaf l = fmap K1 . fromValue l
