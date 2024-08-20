module Zinza.Type (
    Ty (..),
    tyUnit,
    displayTy,
    ) where

import qualified Data.Map as Map

import Zinza.Var

-- $setup
-- >>> import Zinza
-- >>> import Data.Proxy (Proxy (..))

-- | Zinza types.
--
-- The 'selector's tell how the Haskell value can be
-- converted to primitive value. E.g.
--
-- >>> toType (Proxy :: Proxy Char)
-- TyString (Just "return")
--
-- Here, 'return' converts 'Char' to 'String'.
--
data Ty
    = TyBool                                 -- ^ boolean
    | TyString (Maybe Selector)              -- ^ string
    | TyList (Maybe Selector) Ty             -- ^ lists
    | TyRecord (Map.Map Var (Selector, Ty))  -- ^ records
    | TyFun Ty Ty                            -- ^ functions
  deriving (Eq, Ord, Show)

-- | A record without fields is a unit type. Think of zero-field tuple: @()@.
tyUnit :: Ty
tyUnit = TyRecord Map.empty

-- | Pretty print 'Ty'.
displayTy :: Ty -> String
displayTy ty = go 0 ty "" where
    go :: Int -> Ty -> ShowS
    go _ TyBool       = showString "Bool"
    go _ (TyString _) = showString "String"
    go _ (TyList _ t) = showChar '[' . go 0 t . showChar ']'
    go _ (TyRecord m) = case Map.toList m of
        []            -> showString "{}"
        ((n,(_,t)) : nts) -> foldl
            (\acc (n',(_,t')) -> acc . showString ", " . showPair n' t')
            (showChar '{' . showPair n t)
            nts
            . showChar '}'
    go d (TyFun a b) = showParen (d > 10) $
        go 11 a . showString " -> " . go 10 b

    showPair n t = showString n . showString ": " . go 0 t
