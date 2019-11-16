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
    = NRaw  String                          -- ^ raw text block
    | NExpr (Expr a)                        -- ^ expression @expr : String@
    | NIf   (Expr a) (Nodes a)              -- ^ conditional block, @expr : Bool@
    | NFor  Var (Expr a) (Nodes (Maybe a))  -- ^ for loop, @expr : List a@
  deriving (Show, Functor, Foldable, Traversable)

instance TraversableWithLoc Node where
    traverseWithLoc _ (NRaw s)   = pure (NRaw s)
    traverseWithLoc f (NExpr e)  = NExpr <$> traverseWithLoc f e
    traverseWithLoc f (NIf e ns) = NIf
        <$> traverseWithLoc f e
        <*> traverse (traverseWithLoc f) ns
    traverseWithLoc f (NFor v e ns) = NFor v
        <$> traverseWithLoc f e
        <*> traverse (traverseWithLoc f') ns
      where
        f' (L _ Nothing)  = pure Nothing
        f' (L l (Just x)) = Just <$> f (L l x)

-- | Substitution.
(>>==) :: Node a -> (a -> Expr b) -> Node b
NRaw s           >>== _ = NRaw s
NExpr expr       >>== k = NExpr (expr >>= k)
NIf expr ns      >>== k = NIf (expr >>= k) (map (>>== k) ns)
NFor var expr ns >>== k = NFor var (expr >>= k) (map (>>== traverse k) ns)
