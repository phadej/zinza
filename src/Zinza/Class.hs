{-# LANGUAGE ScopedTypeVariables #-}
module Zinza.Class (
    Zinza (..),
    ) where

import Data.Foldable (toList)
import Data.Proxy    (Proxy (..))

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Lazy      as Map
import qualified Data.Set           as Set

import Zinza.Type
import Zinza.Value
--
-- $setup
-- >>> :set -XDeriveGeneric
-- >>> import Data.Proxy (Proxy (..))
-- >>> import GHC.Generics (Generic)
-- >>> import Zinza

-- | 'Zinza' class tells how to convert the type into template parameters,
-- and their types.
--
-- Class can be auto-derived for product types.
--
-- >>> data R = R { recFoo :: String, recBar :: Char } deriving Generic
-- >>> instance Zinza R where toType = genericToTypeSFP; toValue = genericToValueSFP
-- >>> displayTy $ toType (Proxy :: Proxy R)
-- "{bar: String, foo: String}"
--
class Zinza a where
    toType     :: Proxy a -> Ty
    toTypeList :: Proxy a -> Ty
    toTypeList = TyList . toType

    toValue     :: a   -> Value
    toValueList :: [a] -> Value
    toValueList = VList . map toValue

instance Zinza () where
    toType _  = tyUnit
    toValue _ = VRecord mempty

instance Zinza Bool where
    toType _ = TyBool
    toValue = VBool

instance Zinza Char where
    toType     _ = TyString
    toTypeList _ = TyString

    toValue     = VString . return
    toValueList = VString

instance Zinza a => Zinza [a] where
    toType _ = toTypeList (Proxy :: Proxy a)
    toValue = toValueList

instance (Zinza a, Zinza b) => Zinza (a, b) where
    toType _ = TyRecord $ Map.fromList
        [ ("fst", ("fst", toType (Proxy :: Proxy a)))
        , ("snd", ("snd", toType (Proxy :: Proxy b)))
        ]

    toValue (a, b) = VRecord $ Map.fromList
        [ ("fst", toValue a)
        , ("snd", toValue b)
        ]

-------------------------------------------------------------------------------
-- semigroups
-------------------------------------------------------------------------------

instance Zinza a => Zinza (NE.NonEmpty a) where
    toType _ = TyList (toType (Proxy :: Proxy a))
    toValue  = VList . map toValue . toList

-------------------------------------------------------------------------------
-- containers
-------------------------------------------------------------------------------

instance Zinza a => Zinza (Set.Set a) where
    toType _ = TyList (toType (Proxy :: Proxy a))
    toValue  = VList . map toValue . toList

{-
-- | Pairs are encoded as @{key: k, val: v }@
instance (Zinza k, Zinza v) => Zinza (Map.Map k v) where
    toType _ = TyList (toType (Proxy :: Proxy v))
    toValue  = VList . map toValue . toList
-}
