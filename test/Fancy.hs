{-# LANGUAGE DeriveGeneric #-}
module Fancy where

import Data.Map     (Map)
import GHC.Generics (Generic (..))

import Zinza

data Fancy = Fancy
    { fancyBoolA  :: Bool
    , fancyString :: String
    , fancyMap    :: Map String String
    }
  deriving (Generic)

instance Zinza Fancy where
    toType  = genericToTypeSFP
    toValue = genericToValueSFP
