module Main (main) where

import Control.Exception (displayException, throwIO)
import Test.Tasty        (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit  (testCase, (@?=))

import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Map.Strict            as Map

import Fancy
import Licenses

import Zinza

main :: IO ()
main = defaultMain $ testGroup "Zinza"
    [ testCase "example" $ do
          ex <- example
          ex @?= unlines
              [ "licenseName Foo = \"foo-1.0\""
              , "licenseName Bar = \"bar-1.2\""
              ]
    , testGroup "Golden"
        [ testGolden lics licsMC "licenses"
        , testGolden lics licsMC "error-typo"
        , testGolden lics licsMC "error-field"
        , testGolden lics licsMC "error-string"
        , testGolden lics licsMC "regression-c"
        , testGolden fancy fancyMC "fancy"
        ]
    ]
  where
    testGolden :: Zinza a => a -> ModuleConfig a -> TestName -> TestTree
    testGolden input mc name = testGroup name
        [ goldenVsString "txt" ("fixtures/" ++ name ++ ".txt") $ do
            contents <- readFile $ "fixtures/" ++ name ++ ".zinza"
            case parseAndCompileTemplate "" contents of
                Left err -> return (LBS8.pack (displayException err))
                Right run -> case run input of
                    Left err  -> return (LBS8.pack (displayException (err :: RuntimeError)))
                    Right res -> return (LBS8.pack res)
        , goldenVsString "module" ("fixtures/" ++ name ++ ".hs") $ do
            contents <- readFile $ "fixtures/" ++ name ++ ".zinza"
            case parseAndCompileModule mc "" contents of
                Left err  -> return (LBS8.pack (displayException err))
                Right mdl -> return (LBS8.pack mdl)
        ]

-------------------------------------------------------------------------------
-- Licenses
-------------------------------------------------------------------------------

lics :: Licenses
lics = Licenses
    [ License "Foo" (show "foo-1.0")
    , License "Bar" (show "bar-1.2")
    ]

licsMC :: ModuleConfig Licenses
licsMC = ModuleConfig
    { mcRender = "render"
    , mcHeader =
        [ "module Demo (render) where"
        , "import Control.Monad (forM_, when)"
        , "import Control.Monad.Writer (execWriter, tell)"
        , "import Licenses"
        , "render :: Licenses -> String"
        ]
    }

-------------------------------------------------------------------------------
-- Fancy
-------------------------------------------------------------------------------

fancy :: Fancy
fancy = Fancy
    { fancyBoolA  = True
    , fancyString = "fancy string"
    , fancyMap = Map.fromList
        [ ("foo", "Foo")
        , ("bar", "Bar")
        ]
    }

fancyMC :: ModuleConfig Fancy
fancyMC = ModuleConfig
    { mcRender = "render"
    , mcHeader =
        [ "module Demo (render) where"
        , "import Control.Monad (forM_, when)"
        , "import Control.Monad.Writer (execWriter, tell)"
        , "import qualified Data.Map.Strict as Map"
        , "import Fancy"
        , "render :: Fancy -> String"
        ]
    }

-------------------------------------------------------------------------------
-- Example
-------------------------------------------------------------------------------

example :: IO String
example = do
    contents <- readFile "fixtures/licenses.zinza"
    -- this might fail
    run <- runEither $ parseAndCompileTemplate "" contents
    -- this shouldn't fail (run-time errors are due bugs in zinza)
    run $ Licenses
        [ License "Foo" (show "foo-1.0")
        , License "Bar" (show "bar-1.2")
        ]
  where
    runEither (Left err) = throwIO err
    runEither (Right x)  = return x


