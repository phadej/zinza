module Zinza.Var where

import Data.List.NonEmpty (NonEmpty (..), cons)

-- | Variable name, possibly a fieldname in the record.
type Var = String

-- | Haskell selector.
type Selector = String

-- | A very simple haskell expression
newtype HsExpr = HsExpr (NonEmpty Selector)

hsVar :: Selector -> HsExpr
hsVar = HsExpr . pure

access :: HsExpr -> Selector -> HsExpr
access (HsExpr xs) x = HsExpr (cons x xs)

accessMaybe :: HsExpr -> Maybe Selector -> HsExpr
accessMaybe e = maybe e (access e)

displayHsExpr :: HsExpr -> String
displayHsExpr (HsExpr xs) = "(" ++ foldr1 (\a b -> a ++ " $ " ++ b) xs ++ ")"
