{-# LANGUAGE DeriveGeneric #-}
module Main (main) where

import Control.Exception (throwIO)
import Data.Proxy        (Proxy (..))
import GHC.Generics      (Generic (..))
import Zinza

main :: IO ()
main = do
    contents <- readFile "fixtures/licenses.zinza"
    -- this might fail
    run <- runEither $ parseAndCompileTemplate "" contents
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
