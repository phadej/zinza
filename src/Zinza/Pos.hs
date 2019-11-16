{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
module Zinza.Pos where

data Loc = Loc !Int !Int
  deriving (Eq, Show)

zeroLoc :: Loc
zeroLoc = Loc 0 0

displayLoc :: Loc -> String
displayLoc (Loc l c) = show l ++ ":" ++ show c

data Located a = L {-# UNPACK #-} !Loc a
  deriving (Eq, Show, Functor, Foldable, Traversable)

class Traversable t => TraversableWithLoc t where
    traverseWithLoc :: Applicative f => (Located a -> f b) -> t a -> f (t b)
