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
-- Expression syntax has only two structures
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
module Zinza (
    parseAndCompileTemplate,
    parseTemplate,
    -- * Input class
    Zinza (..),
    -- ** Generic deriving
    genericToType,
    genericToValue,
    genericToTypeSFP,
    genericToValueSFP,
    stripFieldPrefix,
    GZinzaType, GZinzaValue, GFieldNames,
    -- * Templates
    Node (..), Nodes, Expr (..),
    -- * Types
    -- | Zinza's type-system is delibarately extremely simple.
    Ty (..),
    prettyTy,
    -- * Values
    -- | 'Value's are passed at run-time, when the template is interpreted.
    -- When compiled to the Haskell module, 'Value's aren't used.
    Value (..),
    -- * Errors
    ParseError (..),
    CompileError (..),
    CompileOrParseError (..),
    RuntimeError (..),
    AsRuntimeError (..),
    ThrowRuntime (..),
    ) where

import Zinza.Check
import Zinza.Errors
import Zinza.Expr
import Zinza.Generic
import Zinza.Node
import Zinza.Parser
import Zinza.Type
import Zinza.Value
import Zinza.Var

-- | Parse and compile the template.
parseAndCompileTemplate
    :: (Zinza a, ThrowRuntime m)
    => FilePath  -- ^ name of the template
    -> String    -- ^ contents of the template
    -> Either CompileOrParseError (a -> m String)
parseAndCompileTemplate name contents =
    case parseTemplate name contents of
        Left err    -> Left (AParseError err)
        Right nodes -> case check nodes of
            Left err' -> Left (ACompileError err')
            Right res -> Right res
