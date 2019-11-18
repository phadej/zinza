module DemoFancy (render) where
import Prelude (String, fst, snd, ($))
import Control.Monad (forM_)
import Fancy
import qualified Data.Map.Strict as Map
type Writer a = (String, a)
tell :: String -> Writer (); tell x = (x, ())
execWriter :: Writer a -> String; execWriter = fst
render :: Fancy -> String
render (z_root) = execWriter $ do
  tell (fancyString $ z_root)
  tell "\n"
  if (fancyBoolA $ z_root)
  then do
    forM_ (Map.toList $ fancyMap $ z_root) $ \z_var0_kv -> do
      tell (fst $ z_var0_kv)
      tell " -- "
      tell (snd $ z_var0_kv)
      tell "\n"
  else do
    if (fancyBoolB $ z_root)
    then do
      tell "Another output\n"
    else do
      tell "Third branch\n"
