{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
module Zinza.Pos where

-- | Location, line and column.
data Loc = Loc !Int !Int
  deriving (Eq, Show)

-- | /Unknown/ location.
zeroLoc :: Loc
zeroLoc = Loc 0 0

-- | Pretty-print location.
displayLoc :: Loc -> String
displayLoc (Loc l c) = show l ++ ":" ++ show c

-- | Located element.
data Located a = L {-# UNPACK #-} !Loc a
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Some containers have location for each element.
class Traversable t => TraversableWithLoc t where
    traverseWithLoc :: Applicative f => (Located a -> f b) -> t a -> f (t b)
