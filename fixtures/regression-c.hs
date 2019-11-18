module Demo (render) where
import Control.Monad (forM_, when)
import Control.Monad.Writer (execWriter, tell)
import Licenses
render :: Licenses -> String
render (z_root) = execWriter $ do
  tell "int main() {\n"
  forM_ (licenses $ z_root) $ \z_var0_license -> do
    tell "  printf(\""
    tell (licenseCon $ z_var0_license)
    tell "\\n\");\n"
  tell "}\n"
