module Zinza.Var where

-- | Variable name, possibly a fieldname in the record.
type Var = String

-- | Haskell selector.
type Selector = String

-- | A very simple haskell expression
data HsExpr
    = HsVar Var
    | HsSel HsExpr Var
    | HsApp HsExpr HsExpr

hsVar :: Selector -> HsExpr
hsVar = HsVar

access :: HsExpr -> Selector -> HsExpr
access = HsSel

accessMaybe :: HsExpr -> Maybe Selector -> HsExpr
accessMaybe e = maybe e (access e)

displayHsExpr :: HsExpr -> String
displayHsExpr expr0 = go 11 expr0 "" where
    go :: Int -> HsExpr -> ShowS
    go _ (HsVar var)
        = showString var
    go d (HsSel e s)
        = showParen (d > 10)
        $ showString s
        . showChar ' '
        . go 11 e
    go d (HsApp f x)
        = showParen (d > 10)
        $ go 10 f
        . showChar ' '
        . go 11 x
