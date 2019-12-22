{-# LANGUAGE DeriveGeneric #-}
module Licenses where

import GHC.Generics    (Generic (..))
import Test.QuickCheck (Arbitrary (..), genericShrink)

import Zinza

newtype Licenses = Licenses { licenses :: [License] }
  deriving (Eq, Show, Generic)

instance Zinza Licenses where
    toType    = genericToType    id
    toValue   = genericToValue   id
    fromValue = genericFromValue id

data License = License
    { licenseCon  :: String
    , licenseName :: String
    }
  deriving (Eq, Show, Generic)

instance Zinza License where
    toType    = genericToTypeSFP
    toValue   = genericToValueSFP
    fromValue = genericFromValueSFP

instance Arbitrary Licenses where
    arbitrary = Licenses <$> arbitrary
    shrink    = genericShrink

instance Arbitrary License where
    arbitrary = License
        <$> arbitrary
        <*> arbitrary
    shrink = genericShrink
