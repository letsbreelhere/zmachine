module Instruction (exec) where

import Control.Monad.State
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
import Zscii

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
execShortOP b = do
  let opcode = b .&. (bit 4 - 1)
  t <- lookupType (testBit b 5, testBit b 4)
  maybe (exec0OP opcode) (exec1OP opcode) t

exec0OP opcode = case opcode of
  0x2 {-print-} -> do ws <- getStringWords
                      liftIO . putStrLn $ decode ws
  0xa {-quit-} -> quit .= True
  _ -> error $ "Got unknown 0OP:" ++ showHex opcode
  where getStringWords = do w <- consumeWord
                            if testBit w 15
                              then return [w]
                              else do ws <- getStringWords
                                      return (w:ws)

exec1OP opcode t = case opcode of
  _ -> error $ "Got unknown 1OP:" ++ showHex opcode ++ " with argument " ++ show t

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
                      shouldReturn <- use $ curFrame.returnValue.to isJust
                      hasQuit <- use quit
                      when (not $ shouldReturn || hasQuit) execLoop
