{-# LANGUAGE DeriveGeneric #-}
module CabalInstall where

import GHC.Generics (Generic (..))

import Zinza

data CabalInstall = CabalInstall
    { ciLib        :: Bool
    , ciMonolithic :: Bool
    }
  deriving (Generic)

instance Zinza CabalInstall where
    toType  = genericToTypeSFP
    toValue = genericToValueSFP
