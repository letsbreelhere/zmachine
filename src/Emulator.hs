{-# LANGUAGE TemplateHaskell #-}

module Emulator where

import Control.Applicative ((<*))
import Control.Lens
import Control.Monad.State
import Data.Array
import Data.Memory
import Data.CallStack
import qualified Data.ByteString as B

data EmuOptions = EmuOptions { _debug :: Bool
                             }
makeLenses ''EmuOptions

data EmuState = EmuState { _callStack  :: CallStack
                         , _memory     :: Memory
                         , _globalVars :: Array Byte Word
                         , _quit       :: Bool
                         , _options    :: EmuOptions
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
                     newGlobals
                     False
                     defaultOptions
  where newMemory = fromByteString bstr
        newCallStack = stackFrame :# []
        stackFrame = newStackFrame (fromIntegral $ wordAt 0x6 newMemory) []
        newGlobals = listArray (0,239) $ repeat 0

peekByteAt :: (Integral a) => a -> Emulator Byte
peekByteAt i = use $ memory.to (byteAt $ fromIntegral i)

peekByte :: Emulator Byte
peekByte = use thePC >>= peekByteAt

consumeByte :: Emulator Byte
consumeByte = peekByte <* (thePC += 1)

peekWordAt :: (Integral a) => a -> Emulator Word
peekWordAt i = use $ memory.to (wordAt $ fromIntegral i)

peekWord :: Emulator Word
peekWord = do
  use thePC >>= peekWordAt

consumeWord :: Emulator Word
consumeWord = peekWord <* (thePC += 2)
