{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveTraversable    #-}
module Zinza.Expr (
    Expr (..),
    abstract1,
    instantiate1ret,
    ) where

import Control.Monad (ap)
import Data.Maybe (fromMaybe)

import Zinza.Var

-------------------------------------------------------------------------------
-- Node syntax
-------------------------------------------------------------------------------

-- | Expressions
data Expr a
    = EVar a               -- ^ variable
    | EField (Expr a) Var  -- ^ field accessor
    | ENot (Expr a)        -- ^ negation
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | 'Monad' instances give substitution.
instance Monad Expr where
    return = EVar

    EVar x          >>= k = k x
    EField expr var >>= k = EField (expr >>= k) var
    ENot expr       >>= k = ENot (expr >>= k)

instance Applicative Expr where
    pure = return
    (<*>) = ap

-------------------------------------------------------------------------------
-- "bound"
-------------------------------------------------------------------------------

-- | Abstraction.
abstract1 :: (Functor f, Eq a) => a -> f a -> f (Maybe a)
abstract1 x = fmap $ \y ->
    if x == y
    then Nothing
    else Just y

-- | Instantiate with a variable type
instantiate1ret :: Functor f => a -> f (Maybe a) -> f a
instantiate1ret = fmap . fromMaybe
