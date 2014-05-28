module Instruction (exec) where

import Control.Monad
import Control.Lens
import Data.Memory
import Data.Maybe
import Emulator
import Util
import qualified Debug as D
import Data.Bits
import Data.ZTypes
import Data.CallStack

exec :: Byte -> Emulator ()
exec b = do
  D.log $ "Executing " ++ showBin b
  case (testBit b 7, testBit b 6) of
    (False,    _) -> exec2OP     b
    (True, False) -> execShortOP b
    (True,  True) -> execVAROP   b

exec2OP :: Byte -> Emulator ()
exec2OP b = case b of
  _ -> error $ "Got unknown 2OP " ++ showHex b

execShortOP :: Byte -> Emulator ()
execShortOP b = case b of
  _ -> error $ "Got unknown short op " ++ showHex b

execVAROP :: Byte -> Emulator ()
execVAROP b = do
  let opcode = b .&. (bit 5 - 1)
  args <- parseTypeByte =<< consumeByte
  doVAROP opcode args

doVAROP opcode args = case opcode of
  0x0 {-call_vs-} -> do let values = map readType args
                        callRoutine (head values) (tail values)
                        return ()
  _ -> error $ "Got unknown VAROP " ++ showHex opcode ++ " with arguments " ++ show args

callRoutine :: Word -> [Word] -> Emulator Word
callRoutine routine args = do
  let raddr = 4 * fromIntegral routine
      newFrame = newStackFrame (raddr + 1)
  D.log $ "Executing routine at " ++ showHex raddr
  callStack %= push newFrame
  execLoop
  ret <- use $ curFrame.returnValue.to fromJust
  callStack %= discard
  return ret
  where execLoop = do instr <- consumeByte
                      exec instr
                      done <- use $ curFrame.returnValue.to isJust
                      when (not done) execLoop
