{-# LANGUAGE TemplateHaskell #-}

module Emulator where

import Control.Lens
import Control.Monad.State
import Data.Memory
import Data.CallStack
import qualified Data.ByteString as B

data EmuState = EmuState { _callStack :: CallStack
                         , _memory    :: Memory
                         }

makeLenses ''EmuState

curFrame :: Lens' EmuState StackFrame
curFrame = callStack.top

thePC :: Lens' EmuState Int
thePC = curFrame.pc

type Emulator a = StateT EmuState IO a

load :: B.ByteString -> EmuState
load bstr = EmuState newCallStack
                     newMemory
  where newMemory = fromByteString bstr
        newCallStack = stackFrame :# []
        stackFrame = newStackFrame (fromIntegral $ wordAt 0x6 newMemory)
