module Emulator.Run (runEmulator) where

import Control.Lens
import Control.Monad.State
import Emulator
import qualified Instruction as Instr

runEmulator :: EmuState -> IO EmuState
runEmulator st = execStateT run st
  where run = do instr <- consumeByte
                 Instr.exec instr
                 done <- use quit
                 when (not done) run
