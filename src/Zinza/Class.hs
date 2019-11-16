{-# LANGUAGE ScopedTypeVariables #-}
module Zinza.Class (
    Zinza (..),
    ) where

import Data.Proxy (Proxy (..))

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
-- >>> prettyTy $ toType (Proxy :: Proxy R)
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
    toType _ = TyRecord mempty
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
