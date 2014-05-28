{-# LANGUAGE TemplateHaskell #-}

module Emulator where

import Control.Applicative ((<*))
import Control.Lens
import Control.Monad.State
import Data.Memory
import Data.CallStack
import qualified Data.ByteString as B

data EmuOptions = EmuOptions { _debug :: Bool
                             }
makeLenses ''EmuOptions

data EmuState = EmuState { _callStack :: CallStack
                         , _memory    :: Memory
                         , _quit      :: Bool
                         , _options   :: EmuOptions
                         }
makeLenses ''EmuState

defaultOptions = EmuOptions False

curFrame :: Lens' EmuState StackFrame
curFrame = callStack.top

thePC :: Lens' EmuState Int
thePC = curFrame.pc

type Emulator a = StateT EmuState IO a

load :: B.ByteString -> EmuState
load bstr = EmuState newCallStack
                     newMemory
                     False
                     defaultOptions
  where newMemory = fromByteString bstr
        newCallStack = stackFrame :# []
        stackFrame = newStackFrame (fromIntegral $ wordAt 0x6 newMemory)

peekByte :: Emulator Byte
peekByte = do
  pc <- use thePC
  use $ memory.to (byteAt pc)

consumeByte :: Emulator Byte
consumeByte = peekByte <* (thePC += 1)

peekWord :: Emulator Word
peekWord = do
  pc <- use thePC
  use $ memory.to (wordAt pc)

consumeWord :: Emulator Word
consumeWord = peekWord <* (thePC += 2)
