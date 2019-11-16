module Zinza.Type where

import qualified Data.Map            as M

import Zinza.Var

data Ty
    = TyUnit
    | TyBool
    | TyString
    | TyList Ty
    | TyRecord (M.Map Var (Selector, Ty))
  deriving (Eq, Ord, Show)

data Path
    = PRoot
    | Path :! Int
    | Path :. Var
  deriving (Show)

infixl 0 :!, :.
