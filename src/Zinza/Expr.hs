{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
module Zinza.Expr (
    Expr (..),
    LExpr,
    abstract1,
    instantiate1ret,
    ) where

import Control.Monad (ap)
import Data.Maybe    (fromMaybe)

import Zinza.Var
import Zinza.Pos

-------------------------------------------------------------------------------
-- Node syntax
-------------------------------------------------------------------------------

-- | Expressions in templates.
--
-- Note: there are only eliminators; we cannot construct "bigger" expressions.
data Expr a
    = EVar (Located a)                -- ^ variable
    | EField (LExpr a) (Located Var)  -- ^ field accessor
    | ENot                            -- ^ negation function
    | EApp (LExpr a) (LExpr a)        -- ^ function application
  deriving (Show, Functor, Foldable, Traversable)

-- | Located expression.
type LExpr a = Located (Expr a)

instance TraversableWithLoc Expr where
    traverseWithLoc f (EVar (L l x)) = EVar . L l
        <$> f l x
    traverseWithLoc f (EField (L l e) v) = (\e' -> EField (L l e') v)
        <$> traverseWithLoc f e
    traverseWithLoc _ ENot = pure ENot
    traverseWithLoc f (EApp (L lx x) (L ly y)) =
        (\x' y' -> EApp (L lx x') (L ly y'))
        <$> traverseWithLoc f x
        <*> traverseWithLoc f y

-- | 'Monad' instance gives substitution.
instance Monad Expr where
    return = EVar . L zeroLoc

    EVar (L _ x)           >>= k = k x
    EField (L l expr) var  >>= k = EField (L l (expr >>= k)) var
    ENot                   >>= _ = ENot
    EApp (L lx x) (L ly y) >>= k = EApp (L lx (x >>= k)) (L ly (y >>= k))             

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
