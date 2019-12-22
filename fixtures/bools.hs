{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module DemoBools (render) where
import Prelude (String, fst, snd, ($), return)
import Control.Monad (forM_)
import Bools
type Writer a = (String, a)
tell :: String -> Writer (); tell x = (x, ())
execWriter :: Writer a -> String; execWriter = fst
render :: Bools -> String
render z_root = execWriter $ do
  tell "Not\n"
  tell "===\n"
  tell "\n"
  forM_ (boolsBools z_root) $ \z_var0_b -> do
    tell "not "
    if z_var0_b
    then do
      tell "1"
      return ()
    else do
      tell "0"
      return ()
    tell " = "
    if (boolsNot z_root z_var0_b)
    then do
      tell "1"
      return ()
    else do
      tell "0"
      return ()
    tell "\n"
  tell "\n"
  tell "And\n"
  tell "===\n"
  tell "\n"
  forM_ (boolsBools z_root) $ \z_var1_x -> do
    forM_ (boolsBools z_root) $ \z_var2_y -> do
      tell "and "
      if z_var1_x
      then do
        tell "1"
        return ()
      else do
        tell "0"
        return ()
      tell " "
      if z_var2_y
      then do
        tell "1"
        return ()
      else do
        tell "0"
        return ()
      tell " = "
      if (boolsAnd z_root z_var1_x z_var2_y)
      then do
        tell "1"
        return ()
      else do
        tell "0"
        return ()
      tell "\n"
