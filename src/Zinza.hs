-- |
-- SPDX-Identifier-Id: GPL-2.0-or-later AND BSD-3-Clause
--
-- Zinza - a small jinja-syntax-inspired template compiler.
--
-- Zinza compiles a template into as Haskell module. The template arguments are
-- ordinary Haskell records. Type-safe (text) templates without Template
-- Haskell, but using a preprocessor.
--
-- Zinza is very minimalistic. Features are added when needed.
--
-- == Examples
--
-- For an example see SPDX LicenseId module generation.
--
-- == Executable usage
--
-- @
-- ./zinza INPUT.tmpl OUTPUT.hs [--module-name=Template]
-- @
--
-- == Syntax
--
-- === Expressions
--
-- @
-- {{ expression }}
-- @
--
-- Expression syntax has two structures
--
-- * negated: @!foo@
--
-- * field access @foo.bar@
--
-- === Control structures
--
-- The __for__ and __if__ statements are supported:
--
-- @
-- {% for value in values %}
-- ...
-- {% endfor %}
-- @
--
-- @
-- {% if boolExpression %}
-- ...
-- {% endif %}
-- @
--
-- If a control structure tag starts at the first column, the possible
-- trailing new line feed is stripped. This way full-line control tags
-- don't introduce new lines in the output.
--
-- == Miscellanea
--
-- A plan is to iterate @zinza@ for some time in @Cabal@ code-base
-- and later release as a separate package.
--
-- Zinza could produce invalid Haskell code.
-- In some cases it could be smarter, or you can edit your template.
--
-- The license is @GPL-2.0-or-later@ with an exception
-- that the code embedded into a generate template is free of it.
-- I use @Bison-exception-2.2@ to indicate that.
--

{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE DeriveGeneric #-}
module Zinza (example) where

import Control.Exception (throwIO)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic (..))

import Zinza.Check
import Zinza.Expr
import Zinza.Node
import Zinza.Parser
import Zinza.Type
import Zinza.Var
import Zinza.Generic

-------------------------------------------------------------------------------
-- Example
-------------------------------------------------------------------------------

example :: IO ()
example = do
    contents <- readFile "fixtures/licenses.zinza"
    nodes <- runEither $ parseTemplate "" contents
    -- this might fail, type error
    run <- runEither (check nodes)
    -- this shouldn't fail (run-time errors are due bugs)
    ss  <- run $ Licenses [License "Foo" "foo-1.0", License "Bar" "bar-1.2"]
    putStrLn ss
  where
    runEither (Left err) = throwIO err
    runEither (Right x)  = return x

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
    toType  = genericToType  (stripFieldPrefix (Proxy :: Proxy License))
    toValue = genericToValue (stripFieldPrefix (Proxy :: Proxy License))
