module Instruction (exec) where

import Control.Lens
import Data.Memory
import Emulator

exec :: Byte -> Emulator ()
exec _ = quit .= True
