module Debug where

import Control.Lens
import Control.Monad.State
import Emulator

log :: String -> Emulator ()
log s = do
  debugOn <- use $ options.debug
  when debugOn . liftIO . putStrLn $ s
