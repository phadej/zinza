module Zinza.Value where

import qualified Data.Map.Strict as M

import Zinza.Var

-- | Template values.
data Value
    = VBool Bool                 -- ^ booleans
    | VString String             -- ^ strings
    | VList [Value]              -- ^ lists
    | VRecord (M.Map Var Value)  -- ^ records
  deriving (Show)
