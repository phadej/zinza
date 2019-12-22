{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module DemoLicenses (render) where
import Prelude (String, fst, snd, ($), return)
import Control.Monad (forM_)
import Licenses
type Writer a = (String, a)
tell :: String -> Writer (); tell x = (x, ())
execWriter :: Writer a -> String; execWriter = fst
render :: Licenses -> String
render z_root = execWriter $ do
  forM_ (licenses z_root) $ \z_var0_license -> do
    tell "licenseName "
    tell (licenseCon z_var0_license)
    tell " = "
    tell (licenseName z_var0_license)
    tell "\n"
