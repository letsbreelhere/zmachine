module Instruction (exec) where

import Control.Lens
import Data.Memory
import Emulator
import Util
import qualified Debug as D
import Data.Bits

exec :: Byte -> Emulator ()
exec b = do
  D.log $ "Executing " ++ showBin b
  case (testBit b 7, testBit b 6) of
    (False,    _) -> exec2OP     b
    (True, False) -> execShortOP b
    (True,  True) -> execVAROP   b

exec2OP b = case b of
  _ -> error $ "Got unknown 2OP " ++ showHex b

execShortOP b = case b of
  _ -> error $ "Got unknown short op " ++ showHex b

execVAROP b = do
  let opcode = b .&. (2^5 - 1)
  typeByte <- consumeByte
  doVAROP opcode []

doVAROP opcode args = case opcode of
  _ -> error $ "Got unknown VAROP " ++ showHex opcode ++ " with arguments " ++ show (map showHex args)
