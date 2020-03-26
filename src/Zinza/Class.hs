{-# LANGUAGE ScopedTypeVariables #-}
module Zinza.Class (
    Zinza (..),
    ) where

import Control.Exception (throw)
import Data.Foldable     (toList)
import Data.Proxy        (Proxy (..))

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Lazy      as Map
import qualified Data.Set           as Set
import qualified Data.Text          as T
import qualified Data.Text.Lazy     as LT

import Zinza.Errors
import Zinza.Pos
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
-- >>> instance Zinza R where toType = genericToTypeSFP; toValue = genericToValueSFP; fromValue = genericFromValueSFP
-- >>> displayTy $ toType (Proxy :: Proxy R)
-- "{bar: String, foo: String}"
--
class Zinza a where
    toType     :: Proxy a -> Ty
    toTypeList :: Proxy a -> Ty
    toTypeList = TyList Nothing . toType

    toValue     :: a   -> Value
    toValueList :: [a] -> Value
    toValueList = VList . map toValue

    fromValue     :: Loc -> Value -> Either RuntimeError a
    fromValueList :: Loc -> Value -> Either RuntimeError [a]
    fromValueList l (VList xs) = traverse (fromValue l) xs
    fromValueList l v          = throwRuntime $ NotList l (valueType v)

instance Zinza () where
    toType _    = tyUnit
    toValue _   = VRecord mempty

    -- we can be strict, but it's easy to just eat some errors.
    fromValue _ _ = return ()

instance Zinza Bool where
    toType _ = TyBool
    toValue = VBool

    fromValue _ (VBool b) = return b
    fromValue l v         = throwRuntime (NotBool l (valueType v))

instance Zinza Char where
    toType     _ = TyString (Just "return")
    toTypeList _ = TyString Nothing

    toValue     = VString . return
    toValueList = VString

    fromValue _ (VString [c]) = return c
    fromValue l v             = throwRuntime $ CustomError l "Not Char" (valueType v)

    fromValueList _ (VString s) = return s
    fromValueList l v           = throwRuntime (NotString l (valueType v))

instance Zinza a => Zinza [a] where
    toType _  = toTypeList (Proxy :: Proxy a)
    toValue   = toValueList
    fromValue = fromValueList

instance (Zinza a, Zinza b) => Zinza (a, b) where
    toType _ = TyRecord $ Map.fromList
        [ ("fst", ("fst", toType (Proxy :: Proxy a)))
        , ("snd", ("snd", toType (Proxy :: Proxy b)))
        ]

    toValue (a, b) = VRecord $ Map.fromList
        [ ("fst", toValue a)
        , ("snd", toValue b)
        ]

    fromValue l (VRecord m)
        | [("fst", x), ("snd", y)] <- Map.toList m
        = (,) <$> fromValue l x <*> fromValue l y
    fromValue l v = throwRuntime $ CustomError l "Not pair" (valueType v)

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

-- | The 'fromValue' for function produces partial functions.
-- Use with care.
--
-- This means that higher order functions in templates might throw
-- pure exception. They wont, if they are well-typed.
--
instance (Zinza a, Zinza b) => Zinza (a -> b) where
    toType _ = TyFun (toType (Proxy :: Proxy a)) (toType (Proxy :: Proxy b))
    toValue f = VFun $ fmap (toValue . f) . fromValue zeroLoc
    fromValue l (VFun f) = return $
        either throw id . (>>= fromValue l) . f . toValue
    fromValue l v = throwRuntime $ NotFunction l (valueType v)

-------------------------------------------------------------------------------
-- semigroups
-------------------------------------------------------------------------------

instance Zinza a => Zinza (NE.NonEmpty a) where
    toType _ = TyList Nothing (toType (Proxy :: Proxy a))
    toValue  = VList . map toValue . toList
    fromValue l v = do
        xs <- fromValue l v
        case xs of
            []     -> throwRuntime $ CustomError l "Not NonEmpty" (valueType v)
            (y:ys) -> return (y NE.:| ys)

-------------------------------------------------------------------------------
-- containers
-------------------------------------------------------------------------------

instance (Zinza a, Ord a) => Zinza (Set.Set a) where
    toType _    = TyList Nothing (toType (Proxy :: Proxy a))
    toValue     = VList . map toValue . toList
    fromValue l = fmap Set.fromList . fromValue l

-- | Pairs are encoded as @{ key: k, val: v }@
instance (Zinza k, Zinza v, Ord k) => Zinza (Map.Map k v) where
    toType _ = TyList (Just "Map.toList") $ TyRecord $ Map.fromList
        [ ("key", ("fst", toType (Proxy :: Proxy k)))
        , ("val", ("snd", toType (Proxy :: Proxy v)))
        ]

    toValue m = VList
        [ VRecord $ Map.fromList
            [ ("key", toValue k)
            , ("val", toValue v)
            ]
        | (k, v) <- Map.toList m
        ]

    fromValue l (VList xs) = do
        kvs <- traverse fromPair xs
        return (Map.fromList kvs)
      where
        fromPair (VRecord m)
            | [("key", x), ("val", y)] <- Map.toList m
            = (,) <$> fromValue l x <*> fromValue l y
        fromPair v = throwRuntime $ CustomError l "Not pair" (valueType v)
    fromValue l v = throwRuntime $ NotList l (valueType v)

-------------------------------------------------------------------------------
-- text
-------------------------------------------------------------------------------

instance Zinza T.Text where
    toType _ = TyString (Just "T.unpack")
    toValue  = VString . T.unpack
    fromValue l = fmap T.pack . fromValue l

instance Zinza LT.Text where
    toType _ = TyString (Just "LT.unpack")
    toValue  = VString . LT.unpack
    fromValue l = fmap LT.pack . fromValue l
