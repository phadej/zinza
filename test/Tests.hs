{-# LANGUAGE DeriveGeneric #-}
module Main (main) where

import Control.Exception (throwIO, displayException)
import Data.Proxy        (Proxy (..))
import GHC.Generics      (Generic (..))
import Test.Tasty        (defaultMain, testGroup)
import Test.Tasty.HUnit  (testCase, (@?=))
import Test.Tasty.Golden (goldenVsString)

import qualified Data.ByteString.Lazy.Char8 as LBS8

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
        [ testGolden "licenses"
        , testGolden "error-typo"
        , testGolden "error-field"
        ]
    ]
  where
    testGolden name = goldenVsString name ("fixtures/" ++ name ++ ".txt") $ do
        contents <- readFile $ "fixtures/" ++ name ++ ".zinza"
        case parseAndCompileTemplate "" contents of
            Left err -> return (LBS8.pack (displayException err))
            Right run -> case run input of
                Left err  -> return (LBS8.pack (displayException (err :: RuntimeError)))
                Right res -> return (LBS8.pack res)

    input = Licenses
        [ License "Foo" (show "foo-1.0")
        , License "Bar" (show "bar-1.2")
        ]

-------------------------------------------------------------------------------
-- Example
-------------------------------------------------------------------------------

example :: IO String
example = do
    contents <- readFile "fixtures/licenses.zinza"
    -- this might fail
    run <- runEither $ parseAndCompileTemplate "" contents
    -- this shouldn't fail (run-time errors are due bugs)
    run $ Licenses
        [ License "Foo" (show "foo-1.0")
        , License "Bar" (show "bar-1.2")
        ]
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
