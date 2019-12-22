{-# LANGUAGE DeriveGeneric #-}
module Bools where

import GHC.Generics    (Generic (..))
import Test.QuickCheck (Arbitrary (..), elements, genericShrink)

import Zinza

-- Worth trying is to 
-- 
-- make load-bools
-- :m +Prelude
-- putStr $ render (Bools [True, False] id (||))

data Bools = Bools
    { boolsBools :: [Bool]
    , boolsNot   :: Bool -> Bool
    , boolsAnd   :: Bool -> Bool -> Bool
    }
  deriving (Generic)

instance Eq Bools where
    Bools x0 x1 x2 == Bools y0 y1 y2 = and
        [ x0 == y0
        , eqFun1 x1 y1
        , eqFun2 x2 y2
        ]

instance Zinza Bools where
    toType    = genericToTypeSFP
    toValue   = genericToValueSFP
    fromValue = genericFromValueSFP

instance Arbitrary Bools where
    arbitrary = Bools
        <$> arbitrary
        <*> elements unaryBools
        <*> elements binaryBools

    shrink = genericShrink

unaryBools :: [Bool -> Bool]
unaryBools = 
    [ id
    , const True
    , const False
    , not
    ] 

binaryBools :: [Bool -> Bool -> Bool]
binaryBools = 
    [ \_ _ -> True
    , \_ _ -> False
    , (&&)
    , (||)
    , \x _ -> x
    , \x _ -> not x
    , \_ y -> y
    , \_ y -> not y
    ]

eqFun1 :: Eq a => (Bool -> a) -> (Bool -> a) -> Bool
eqFun1 f g = and
    [ f x == g x
    | x <- [True, False]
    ]

eqFun2 :: Eq a => (Bool -> Bool -> a) -> (Bool -> Bool -> a) -> Bool
eqFun2 f g = and
    [ f x y == g x y
    | x <- [True, False]
    , y <- [True, False]
    ]
