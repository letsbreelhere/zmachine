module Data.ZTypes ( ZType(..)
                   , parseTypeByte
                   , lookupType
                   , readType
                   ) where

import Data.Maybe
import Data.Memory
import Data.Bits
import Emulator
import Util

data ZType = ZWord Word
           | ZByte Byte
           | ZVar Byte Word

instance Show ZType where
  show (ZWord      w) = "ZWord " ++ showHex w
  show (ZByte      b) = "ZByte " ++ showHex b
  show (ZVar var val) = "ZVar "  ++ showHex var ++ " " ++ showHex val

getVar :: Byte -> Emulator Word
getVar n = error "getVar"

readType :: ZType -> Word
readType t = case t of
  ZWord w    -> w
  ZByte b    -> fromIntegral b
  ZVar _ val -> val

lookupType :: (Bool,Bool) -> Emulator (Maybe ZType)
lookupType pair = case pair of
  (False, False) -> fmap (Just . ZWord) consumeWord
  (False,  True) -> fmap (Just . ZByte) consumeByte
  (True,  False) -> do var <- consumeByte
                       val <- getVar var
                       return . Just $ ZVar var val
  _              -> return Nothing

parseTypeByte :: Byte -> Emulator [ZType]
parseTypeByte b = let topTwoBits b' = (testBit b' 7, testBit b' 6)
                      bitPairs = takeWhile (/= (True,True)) . map topTwoBits . take 4 $ iterate (`shiftL` 2) b
                  in fmap (map fromJust) . mapM lookupType $ bitPairs
