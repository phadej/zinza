module Zinza.Type (
    Ty (..),
    tyUnit,
    displayTy,
    ) where

import qualified Data.Map as M

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
-- TBW
--
data Ty
    = TyBool                                 -- ^ boolean
    | TyString (Maybe Selector)              -- ^ string
    | TyList (Maybe Selector) Ty             -- ^ lists
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
    go (TyString _) = showString "String"
    go (TyList _ t) = showChar '[' . go t . showChar ']'
    go (TyRecord m) = case M.toList m of
        []            -> showString "{}"
        ((n,(_,t)) : nts) -> foldl
            (\acc (n',(_,t')) -> acc . showString ", " . showPair n' t')
            (showChar '{' . showPair n t)
            nts
            . showChar '}'

    showPair n t = showString n . showString ": " . go t
