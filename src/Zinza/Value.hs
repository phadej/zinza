module Zinza.Value where

import qualified Data.Map.Strict as M

import Zinza.Var

data Value
    = VUnit
    | VBool Bool
    | VString String
    | VList [Value]
    | VRecord (M.Map Var Value)
  deriving (Show)
