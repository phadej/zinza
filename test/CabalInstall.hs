{-# LANGUAGE DeriveGeneric #-}
module CabalInstall where

import GHC.Generics    (Generic (..))
import Test.QuickCheck (Arbitrary (..), genericShrink)

import Zinza

data CabalInstall = CabalInstall
    { ciLib        :: Bool
    , ciMonolithic :: Bool
    }
  deriving (Eq, Show, Generic)

instance Zinza CabalInstall where
    toType    = genericToTypeSFP
    toValue   = genericToValueSFP
    fromValue = genericFromValueSFP

instance Arbitrary CabalInstall where
    arbitrary = CabalInstall
        <$> arbitrary
        <*> arbitrary

    shrink = genericShrink
