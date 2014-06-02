{-# LANGUAGE TemplateHaskell, FlexibleContexts, RankNTypes #-}

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
type Emulator' a = (MonadState EmuState m) => m a

load :: B.ByteString -> EmuState
load bstr = EmuState newCallStack
                     newMemory
                     False
                     defaultOptions
  where newMemory = fromByteString bstr
        newCallStack = stackFrame :# []
        stackFrame = newStackFrame (fromIntegral $ wordAt 0x6 newMemory) [] 0

globalVar :: Byte -> Emulator' Word
globalVar b = do globalsStart <- use $ memory.to (wordAt 0xc)
                 use $ memory.to (wordAt $ fromIntegral globalsStart + fromIntegral b)

setGlobalVar :: Byte -> Word -> Emulator' ()
setGlobalVar b w = do globalsStart <- use $ memory.to (wordAt 0xc)
                      let varAddr = fromIntegral globalsStart + fromIntegral b
                      memory %= writeWord varAddr w

peekByteAt :: (Integral a) => a -> Emulator' Byte
peekByteAt i = use $ memory.to (byteAt $ fromIntegral i)

peekByte :: Emulator' Byte
peekByte = use thePC >>= peekByteAt

consumeByte :: Emulator' Byte
consumeByte = do b <- peekByte
                 thePC += 1
                 return b

putByte :: Byte -> Emulator' ()
putByte b = do addr <- use thePC
               memory %= writeByte addr b
               thePC += 1

putWord :: Word -> Emulator' ()
putWord w = do let (b1,b2) = bytes w
               putByte b1
               putByte b2

peekWordAt :: (Integral a) => a -> Emulator' Word
peekWordAt i = use $ memory.to (wordAt $ fromIntegral i)

peekWord :: Emulator' Word
peekWord = do
  use thePC >>= peekWordAt

consumeWord :: Emulator' Word
consumeWord = do w <- peekWord
                 thePC += 2
                 return w

withTmpPC :: Int -> Emulator' a -> Emulator' a
withTmpPC p action = do oldPC <- use thePC
                        thePC .= p
                        res <- action
                        thePC .= oldPC
                        return res
