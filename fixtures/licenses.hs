module Demo (render) where
import Control.Monad (forM_, when)
import Control.Monad.Writer (execWriter, tell)
import Licenses
render :: Licenses -> String
render (z_root) = execWriter $ do
  forM_ (licenses $ z_root) $ \z_var0_license -> do
    tell "licenseName "
    tell (licenseCon $ z_var0_license)
    tell " = "
    tell (licenseName $ z_var0_license)
    tell "\n"
