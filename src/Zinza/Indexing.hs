{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
module Zinza.Indexing where

import Data.Functor.Identity (Identity (..))

class Indexing v i | i -> v, v -> i where
    extract :: i a -> a
    index   :: v a -> i b -> (a, b)

instance Indexing Identity Identity where
    extract = runIdentity
    index (Identity a) (Identity b) = (a, b)

data Idx f a
    = Here a
    | There (f a)

data Cons f a = a ::: f a

instance Indexing v i => Indexing (Cons v) (Idx i) where
    extract (Here b)   = b
    extract (There bs) = extract bs

    index (a ::: _)  (Here b)   = (a, b)
    index (_ ::: as) (There bs) = index as bs
