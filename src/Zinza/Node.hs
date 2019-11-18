{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
module Zinza.Node (
    Nodes,
    Node (..),
    (>>==),
    ) where

import Zinza.Expr
import Zinza.Var
import Zinza.Pos

-- | A list of 'Node's.
type Nodes a = [Node a]

-- | Template parts.
--
-- We use polymorphic recursion for de Bruijn indices.
-- See materials on @bound@ library.
--
data Node a
    = NRaw  String                           -- ^ raw text block
    | NExpr (LExpr a)                        -- ^ expression @expr : String@
    | NIf   (LExpr a) (Nodes a) (Nodes a)   -- ^ conditional block, @expr : Bool@
    | NFor  Var (LExpr a) (Nodes (Maybe a))  -- ^ for loop, @expr : List a@
    | NComment                               -- ^ comments
  deriving (Show, Functor, Foldable, Traversable)

instance TraversableWithLoc Node where
    traverseWithLoc _ NComment   = pure NComment
    traverseWithLoc _ (NRaw s)   = pure (NRaw s)
    traverseWithLoc f (NExpr e)  = NExpr
        <$> traverse (traverseWithLoc f) e
    traverseWithLoc f (NIf e xs ys) = NIf
        <$> traverse (traverseWithLoc f) e
        <*> traverse (traverseWithLoc f) xs
        <*> traverse (traverseWithLoc f) ys
    traverseWithLoc f (NFor v e ns) = NFor v
        <$> traverse (traverseWithLoc f) e
        <*> traverse (traverseWithLoc f') ns
      where
        f' _ Nothing  = pure Nothing
        f' l (Just x) = Just <$> f l x

-- | Substitution.
(>>==) :: Node a -> (a -> Expr b) -> Node b
NComment               >>== _ = NComment
NRaw s                 >>== _ = NRaw s
NExpr (L l expr)       >>== k = NExpr (L l (expr >>= k))
NIf (L l expr) xs ys   >>== k = NIf (L l (expr >>= k)) (map (>>== k) xs) (map (>>== k) ys)
NFor var (L l expr) ns >>== k = NFor var (L l (expr >>= k)) (map (>>== traverse k) ns)
