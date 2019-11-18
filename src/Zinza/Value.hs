module Zinza.Value where

import qualified Data.Map.Strict as M

import Zinza.Var
import Zinza.Type

-- | Template values.
data Value
    = VBool Bool                 -- ^ booleans
    | VString String             -- ^ strings
    | VList [Value]              -- ^ lists
    | VRecord (M.Map Var Value)  -- ^ records
  deriving (Show)

-- | Calculate 'Ty' of the 'Value'.
-- This is only an approximation, for list we look at first
-- element if it exists, otherwise we use 'unitTy'.
valueType :: Value -> Ty
valueType (VBool _)     = TyBool
valueType (VString _)   = TyString
valueType (VList [])    = TyList Nothing tyUnit
valueType (VList (v:_)) = TyList Nothing (valueType v)
valueType (VRecord m)   = TyRecord (fmap (\v -> ("", valueType v)) m)
