module Data.ZTypes ( ZType(..)
                   , parseTypeByte
                   ) where

import Data.Memory
import Data.Bits
import Emulator
import Util

data ZType = ZWord Word
           | ZByte Byte
           | ZVar Byte

instance Show ZType where
  show (ZWord w) = "ZWord " ++ showHex w
  show (ZByte b) = "ZByte " ++ showHex b
  show (ZVar  v) = "ZVar "  ++ showHex v

lookupType :: (Bool,Bool) -> Emulator ZType
lookupType pair = case pair of
  (False, False) -> fmap ZWord consumeWord
  (False,  True) -> fmap ZByte consumeByte
  (True,  False) -> fmap ZVar  consumeByte
  _              -> error "Got type bits %11!"

parseTypeByte :: Byte -> Emulator [ZType]
parseTypeByte b = let topTwoBits b' = (testBit b' 7, testBit b' 6)
                      bitPairs = takeWhile (/= (True,True)) . map topTwoBits . take 4 $ iterate (`shiftL` 2) b
                  in mapM lookupType bitPairs
