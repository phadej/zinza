{-# LANGUAGE DeriveGeneric #-}
module Licenses where

import GHC.Generics      (Generic (..))

import Zinza

newtype Licenses = Licenses { licenses :: [License] }
  deriving (Generic)

instance Zinza Licenses where
    toType  = genericToType  id
    toValue = genericToValue id

data License = License
    { licenseCon  :: String
    , licenseName :: String
    }
  deriving (Show, Generic)

instance Zinza License where
    toType  = genericToTypeSFP
    toValue = genericToValueSFP
