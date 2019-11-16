module Zinza.Errors where

import Control.Exception (Exception (..), throwIO)

import Zinza.Var
import Zinza.Pos

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
    = UnboundTopLevelVar Loc Var
    | ARuntimeError RuntimeError
  deriving (Show)

instance Exception CompileError

-------------------------------------------------------------------------------
-- CompileOrParseError
-------------------------------------------------------------------------------

data CompileOrParseError
    = ACompileError CompileError
    | AParseError ParseError
  deriving (Show)

instance Exception CompileOrParseError

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

-- | Class representing errors containing 'RuntimeError's.
--
-- Without bugs, compiled template should not throw any 'RuntimeError's,
-- as they are prevented statically, i.e. reported already as 'CompileError's.
--
class    AsRuntimeError e where asRuntimeError :: RuntimeError -> e
instance AsRuntimeError RuntimeError where asRuntimeError = id
instance AsRuntimeError CompileError where asRuntimeError = ARuntimeError

class Monad m => ThrowRuntime m where
    throwRuntime ::  RuntimeError -> m a

instance AsRuntimeError e => ThrowRuntime (Either e) where
    throwRuntime = Left . asRuntimeError

instance ThrowRuntime IO where
    throwRuntime = throwIO
