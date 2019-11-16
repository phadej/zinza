{-# LANGUAGE ScopedTypeVariables #-}
module Zinza.Class (
    Zinza (..),
    ) where

import Data.Proxy (Proxy (..))

import Zinza.Type
import Zinza.Value

class Zinza a where
    toType     :: Proxy a -> Ty
    toTypeList :: Proxy a -> Ty
    toTypeList = TyList . toType

    toValue     :: a   -> Value
    toValueList :: [a] -> Value
    toValueList = VList . map toValue

instance Zinza () where
    toType _ = TyUnit
    toValue _ = VUnit

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
