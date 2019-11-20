{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- SPDX-Identifier-Id: GPL-2.0-or-later AND BSD-3-Clause
--
-- Zinza - a small jinja-syntax-inspired typed-template compiler.
--
-- Zinza typechecks and compiles a template.
-- We can compile either to Haskell function, or to verbatim Haskell module (planned).
--
-- Zinza is very minimalistic. Features are added when needed.
--
-- == Example usage
--
-- Given a template
--
-- @
-- {% for license in licenses %}
-- licenseName {{license.con}} = {{license.name}}
-- {% endfor %}
-- @
--
-- and data definitions like:
--
-- @
-- newtype Licenses = Licenses { licenses :: [License] }
--   deriving (Generic)
--
-- data License = License
--     { licenseCon  :: String
--     , licenseName :: String
--     }
--   deriving (Generic)
-- @
--
-- We can (generically) derive 'Zinza' instances for @Licenses@ and @License@
--
-- @
-- instance 'Zinza' Licenses where
--     'toType'  = 'genericToType'  id
--     'toValue' = 'genericToValue' id
--
-- instance 'Zinza' License where
--     'toType'  = 'genericToTypeSFP'
--     'toValue' = 'genericToValueSFP'
-- @
--
-- Then the example of run-time usage is
--
-- @
-- example :: IO String
-- example = do
--     -- this might fail, type errors!
--     run <- 'parseAndCompileTemplateIO' "fixtures/licenses.zinza"
--     -- this shouldn't fail (run-time errors are due bugs in zinza)
--     run $ Licenses
--         [ License \"Foo" (show "foo-1.0")
--         , License \"Bar" (show "bar-1.2")
--         ]
-- @
--
-- The result of running an @example@ is:
--
-- @
-- licenseName Foo = "foo-1.0"
-- licenseName Bar = "bar-1.2"
-- @
--
-- == Module generation
--
-- Zinza also supports standalone module generation.
--
-- @
-- 'parseAndCompileModuleIO' ('simpleConfig' \"DemoLicenses\" [\"Licenses\"] :: 'ModuleConfig' Licenses) "fixtures/licenses.zinza" >>= putStr
-- @
--
-- prints a Haskell module source code:
--
-- @
-- module DemoLicenses (render) where
-- import Prelude (String, fst, snd, ($))
-- import Control.Monad (forM_)
-- import Licenses
-- type Writer a = (String, a)
-- tell :: String -> Writer (); tell x = (x, ())
-- execWriter :: Writer a -> String; execWriter = fst
-- render :: Licenses -> String
-- render (z_root) = execWriter $ do
--   forM_ (licenses $ z_root) $ \z_var0_license -> do
--     tell "licenseName "
--     tell (licenseCon $ z_var0_license)
--     tell " = "
--     tell (licenseName $ z_var0_license)
--     tell "\n"
-- @
--
-- which is not dependent on Zinza. You are free to use more efficient writer
-- as well.
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
-- {% elif anotherBoolExpression %}
-- ...
-- {% else %}
-- ...
-- {% endif %}
-- @
--
-- If a control structure tag starts at the first column, the possible
-- trailing new line feed is stripped. This way full-line control tags
-- don't introduce new lines in the output.
--
-- === Comments
--
-- @
-- {\# Comments are omitted from the output #}
-- @
--
module Zinza (
    parseAndCompileTemplate,
    parseAndCompileTemplateIO,
    -- * Compilation to Haskell module
    parseAndCompileModule,
    parseAndCompileModuleIO,
    ModuleConfig (..),
    simpleConfig,
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
    Node (..), Nodes, Expr (..), LExpr,
    -- * Types
    -- | Zinza's type-system is delibarately extremely simple.
    Ty (..),
    displayTy,
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
    -- * Location
    Loc (..), Located (..), zeroLoc, displayLoc, TraversableWithLoc (..),
    -- * Variables
    Var, Selector,
    ) where

import Control.Exception (throwIO)
import Data.Typeable     (Typeable, typeRep)

import Zinza.Check
import Zinza.Errors
import Zinza.Expr
import Zinza.Generic
import Zinza.Module
import Zinza.Node
import Zinza.Parser
import Zinza.Pos
import Zinza.Type
import Zinza.Value
import Zinza.Var

-- | Parse and compile the template into Haskell function.
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

-- | Like 'parseAndCompileTemplate' but reads file and (possibly)
-- throws 'CompileOrParseError'.
parseAndCompileTemplateIO :: (Zinza a, ThrowRuntime m) => FilePath -> IO (a -> m String)
parseAndCompileTemplateIO name = do
    contents <- readFile name
    either throwIO return $ parseAndCompileTemplate name contents

-- | Parse and compile the template into 'String' representing a Haskell module.
parseAndCompileModule
    :: Zinza a
    => ModuleConfig a
    -> FilePath
    -> String
    -> Either CompileOrParseError String
parseAndCompileModule mc name contents =
    case parseTemplate name contents of
        Left err -> Left (AParseError err)
        Right nodes -> case checkModule mc nodes of
            Left err  -> Left (ACompileError err)
            Right res -> Right res

-- | Like 'parseAndCompileModule' but reads file and (possibly)
-- throws 'CompileOrParseError'.
parseAndCompileModuleIO :: Zinza a => ModuleConfig a -> FilePath -> IO String
parseAndCompileModuleIO mc name = do
    contents <- readFile name
    either throwIO return $ parseAndCompileModule mc name contents

-- | Simple configuration to use with 'parseAndCompileModule' or
-- 'parseAndCompileModuleIO'.
simpleConfig
    :: forall a. Typeable a
    => String    -- ^ module name
    -> [String]  -- ^ imports
    -> ModuleConfig a
simpleConfig moduleName imports = ModuleConfig
    { mcRender = "render"
    , mcHeader =
        [ "module " ++ moduleName ++ " (render) where"
        , "import Prelude (String, fst, snd, ($))"
        , "import Control.Monad (forM_)"
        ] ++
        [ "import " ++ i
        | i <- imports
        ] ++
        [ "type Writer a = (String, a)"
        , "tell :: String -> Writer (); tell x = (x, ())"
        , "execWriter :: Writer a -> String; execWriter = fst"
        , "render :: " ++ typeName ++ " -> String"
        ]
    }
  where
    typeName = show (typeRep ([] :: [a]))
