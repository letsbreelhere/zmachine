module Instruction (exec) where

import Control.Lens
import Data.Memory
import Emulator
import Util
import qualified Debug as D

exec :: Byte -> Emulator ()
exec b = do
  D.log $ "Executing " ++ showBin b
  quit .= True
