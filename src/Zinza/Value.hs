module Zinza.Value where

import qualified Data.Map.Strict as Map

import Zinza.Errors
import Zinza.Type
import Zinza.Var

-- | Template values.
data Value
    = VBool Bool                                 -- ^ booleans
    | VString String                             -- ^ strings
    | VList [Value]                              -- ^ lists
    | VRecord (Map.Map Var Value)                  -- ^ records
    | VFun (Value -> Either RuntimeError Value)  -- ^ function

-- | Calculate 'Ty' of the 'Value'.
-- This is only an approximation, for list we look at first
-- element if it exists, otherwise we use 'unitTy'.
valueType :: Value -> Ty
valueType (VBool _)     = TyBool
valueType (VString _)   = TyString Nothing
valueType (VList [])    = TyList Nothing tyUnit
valueType (VList (v:_)) = TyList Nothing (valueType v)
valueType (VRecord m)   = TyRecord (fmap (\v -> ("", valueType v)) m)
valueType (VFun _)      = TyFun tyUnit tyUnit
