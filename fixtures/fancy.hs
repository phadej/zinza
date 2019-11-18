module Demo (render) where
import Control.Monad (forM_, when)
import Control.Monad.Writer (execWriter, tell)
import qualified Data.Map.Strict as Map
import Fancy
render :: Fancy -> String
render (z_root) = execWriter $ do
  tell (fancyString $ z_root)
  tell "\n"
  when (fancyBoolA $ z_root) $ do
    forM_ (Map.toList $ fancyMap $ z_root) $ \z_var0_kv -> do
      tell (fst $ z_var0_kv)
      tell " -- "
      tell (snd $ z_var0_kv)
      tell "\n"
