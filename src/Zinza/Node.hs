{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveTraversable    #-}
module Zinza.Node where

import Zinza.Expr
import Zinza.Var

type Nodes a = [Node a]

-- | Template parts.
--
-- We use polymorphic recursion for de Bruijn indices.
-- See materials on @bound@ library.
data Node a
    = NRaw  String                          -- ^ raw text block
    | NExpr (Expr a)                        -- ^ expression @expr : String@
    | NIf   (Expr a) (Nodes a)              -- ^ conditional block, @expr : Bool@
    | NFor  Var (Expr a) (Nodes (Maybe a))  -- ^ for loop, @expr : List a@
  deriving (Eq, Show, Functor, Foldable, Traversable)


-- | Substitution.
(>>==) :: Node a -> (a -> Expr b) -> Node b
NRaw s           >>== _ = NRaw s
NExpr expr       >>== k = NExpr (expr >>= k)
NIf expr ns      >>== k = NIf (expr >>= k) (map (>>== k) ns)
NFor var expr ns >>== k = NFor var (expr >>= k) (map (>>== traverse k) ns)
