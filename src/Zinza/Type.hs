module Zinza.Type (
    Ty (..),
    tyUnit,
    displayTy,
    ) where

import qualified Data.Map as M

import Zinza.Var

-- | Zinza types.
data Ty
    = TyBool                                 -- ^ boolean
    | TyString                               -- ^ string
    | TyList Ty                              -- ^ lists
    | TyRecord (M.Map Var (Selector, Ty))    -- ^ records
  deriving (Eq, Ord, Show)

-- | A record without fields is a unit type. Think of zero-field tuple: @()@.
tyUnit :: Ty
tyUnit = TyRecord M.empty

-- | Pretty print 'Ty'.
displayTy :: Ty -> String
displayTy ty = go ty "" where
    go :: Ty -> ShowS
    go TyBool       = showString "Bool"
    go TyString     = showString "String"
    go (TyList t)   = showChar '[' . go t . showChar ']'
    go (TyRecord m) = case M.toList m of
        []            -> showString "{}"
        ((n,(_,t)) : nts) -> foldl
            (\acc (n',(_,t')) -> acc . showString ", " . showPair n' t')
            (showChar '{' . showPair n t)
            nts
            . showChar '}'

    showPair n t = showString n . showString ": " . go t
