{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Control.Exception (displayException, throwIO)
import Test.Tasty        (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set (Set)
import Test.Tasty.HUnit  (testCase, (@?=))
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeRep)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Arbitrary, (===), Property)
import Test.QuickCheck.Instances ()

import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Map.Strict            as Map

import Bools
import CabalInstall
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
    , testGroup "Roundtrip"
        [ roundtrip (Proxy :: Proxy ())
        , roundtrip (Proxy :: Proxy Bool)
        , roundtrip (Proxy :: Proxy Char)
        , roundtrip (Proxy :: Proxy String)
        , roundtrip (Proxy :: Proxy (NonEmpty Bool))
        , roundtrip (Proxy :: Proxy (Set String))
        , roundtrip (Proxy :: Proxy (Map.Map String Bool))
        -- custom types
        , roundtrip (Proxy :: Proxy CabalInstall)
        -- , roundtrip (Proxy :: Proxy Fancy)
        -- , roundtrip (Proxy :: Proxy Licenses)
        ]
    , testGroup "Golden"
        [ testGolden lics licsMC "licenses"
        , testGolden lics licsMC "error-typo"
        , testGolden lics licsMC "error-field"
        , testGolden lics licsMC "error-string"
        , testGolden lics licsMC "regression-c"
        , testGolden fancy fancyMC "fancy"
        , testGolden cabal cabalMC "cabal-install"
        , testGolden bools boolsMC "bools"
        ]
    ]
  where
    testGolden :: Zinza a => a -> ModuleConfig a -> TestName -> TestTree
    testGolden input mc name = testGroup name
        [ goldenVsStringDiff "txt" diff ("fixtures/" ++ name ++ ".txt") $ do
            contents <- readFile $ "fixtures/" ++ name ++ ".zinza"
            case parseAndCompileTemplate "" contents of
                Left err -> return (LBS8.pack (displayException err))
                Right run -> case run input of
                    Left err  -> return (LBS8.pack (displayException (err :: RuntimeError)))
                    Right res -> return (LBS8.pack res)
        , goldenVsStringDiff "module" diff ("fixtures/" ++ name ++ ".hs") $ do
            contents <- readFile $ "fixtures/" ++ name ++ ".zinza"
            case parseAndCompileModule mc "" contents of
                Left err  -> return (LBS8.pack (displayException err))
                Right mdl -> return (LBS8.pack mdl)
        ]

    diff :: FilePath -> FilePath -> [String]
    diff ref new = ["diff", "-u", ref, new]

-------------------------------------------------------------------------------
-- Licenses
-------------------------------------------------------------------------------

lics :: Licenses
lics = Licenses
    [ License "Foo" (show "foo-1.0")
    , License "Bar" (show "bar-1.2")
    ]

licsMC :: ModuleConfig Licenses
licsMC = simpleConfig "DemoLicenses" ["Licenses"]

-------------------------------------------------------------------------------
-- Fancy
-------------------------------------------------------------------------------

fancy :: Fancy
fancy = Fancy
    { fancyBoolA  = True
    , fancyBoolB  = True
    , fancyString = "fancy string"
    , fancyMap = Map.fromList
        [ ("foo", "Foo")
        , ("bar", "Bar")
        ]
    , fancyNot = not
    }

fancyMC :: ModuleConfig Fancy
fancyMC = simpleConfig "DemoFancy" ["Fancy", "qualified Data.Map.Strict as Map"]

-------------------------------------------------------------------------------
-- cabal-install
-------------------------------------------------------------------------------

cabal :: CabalInstall
cabal = CabalInstall
    { ciLib = False
    , ciMonolithic = False
    }

cabalMC :: ModuleConfig CabalInstall
cabalMC = simpleConfig "DemoCabalInstall" ["CabalInstall"]

-------------------------------------------------------------------------------
-- Bools
-------------------------------------------------------------------------------

bools :: Bools
bools = Bools
    { boolsBools = [False, True]
    , boolsNot   = not
    , boolsAnd   = (&&)
    }

boolsMC :: ModuleConfig Bools
boolsMC = simpleConfig "DemoBools" ["Bools"]

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

-------------------------------------------------------------------------------
-- roundtrip
-------------------------------------------------------------------------------

roundtrip
    :: forall a. (Typeable a, Zinza a, Eq a, Show a, Arbitrary a)
    => Proxy a -> TestTree
roundtrip p = testProperty (show (typeRep p)) prop where
    prop :: a -> Property
    prop x = fromValue zeroLoc (toValue x) === Right x
