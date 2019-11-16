module Zinza.Errors where

import Control.Exception (Exception (..), throwIO)

import Zinza.Var
import Zinza.Type
import Zinza.Value

-------------------------------------------------------------------------------
-- ParseError
-------------------------------------------------------------------------------

newtype ParseError = ParseError String
  deriving (Show)

instance Exception ParseError where
    displayException (ParseError err) = err

-------------------------------------------------------------------------------
-- CompileError
-------------------------------------------------------------------------------

data CompileError
    = UnboundTopLevelVar Var
    | RuntimeError RuntimeError
  deriving (Show)

instance Exception CompileError

-------------------------------------------------------------------------------
-- RuntimeError
-------------------------------------------------------------------------------

data RuntimeError 
    = NotBool
    | NotString
    | NotRecord
    | NotList
    | FieldNotInRecord Var
  deriving Show

instance Exception RuntimeError

class    AsRuntimeError e where asRuntimeError :: RuntimeError -> e
instance AsRuntimeError RuntimeError where asRuntimeError = id
instance AsRuntimeError CompileError where asRuntimeError = RuntimeError

class Monad m => ThrowRuntime m where
    throwRuntime ::  RuntimeError -> m a

instance AsRuntimeError e => ThrowRuntime (Either e) where
    throwRuntime = Left . asRuntimeError

instance ThrowRuntime IO where
    throwRuntime = throwIO
