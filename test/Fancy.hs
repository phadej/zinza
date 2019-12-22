{-# LANGUAGE DeriveGeneric #-}
module Fancy where

import Data.Map        (Map)
import GHC.Generics    (Generic (..))
import Test.QuickCheck (Arbitrary (..), elements, genericShrink)

import Zinza

data Fancy = Fancy
    { fancyBoolA  :: Bool
    , fancyBoolB  :: Bool
    , fancyString :: String
    , fancyMap    :: Map String String
    , fancyNot    :: Bool -> Bool
    }
  deriving (Generic)

instance Eq Fancy where
    Fancy x0 x1 x2 x3 x4 == Fancy y0 y1 y2 y3 y4 = and
        [ x0 == y0
        , x1 == y1
        , x2 == y2
        , x3 == y3
        , x4 True == y4 True
        , x4 False == y4 False
        ]

instance Zinza Fancy where
    toType    = genericToTypeSFP
    toValue   = genericToValueSFP
    fromValue = genericFromValueSFP

instance Arbitrary Fancy where
    arbitrary = Fancy
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> elements
            [ id
            , const True
            , const False
            , not
            ] 

    shrink = genericShrink
