module Data.ZTypes ( ZType(..)
                   , parseTypeByte
                   , parseTypeWord
                   , lookupType
                   , readType
                   , getVar
                   , setVar
                   , setVarType
                   , setResult
                   ) where

import Control.Lens
import Control.Monad
import Data.CallStack
import Data.Maybe
import Data.Memory
import Data.Bits
import Emulator
import Util
import Data.Array ((!), (//))

data ZType = ZWord Word
           | ZByte Byte
           | ZVar Byte Word

instance Show ZType where
  show (ZWord      w) = "ZWord " ++ showHex w
  show (ZByte      b) = "ZByte " ++ showHex b
  show (ZVar var val) = "ZVar "  ++ showHex var ++ " " ++ showHex val

getVar :: Byte -> Emulator Word
getVar n
  | n == 0 = do stack <- use $ curFrame.localStack
                when (null stack) $ error "Tried to pull from an empty stack!"
                let v = head stack
                curFrame.localStack %= tail
                return v
  | n < 16 = do locals <- use (curFrame.localVars)
                return $ locals ! (n-1)
  | otherwise = globalVar (n-16)

setVarType :: ZType -> ZType -> Emulator ()
setVarType x y = do let xval = readType x
                        yval = readType y
                    setVar (fromIntegral xval) yval

setResult :: Word -> Emulator ()
setResult w = do resultVar <- consumeByte
                 setVar resultVar w

setVar :: Byte -> Word -> Emulator ()
setVar n v
  | n == 0 = curFrame.localStack %= (v:)
  | n < 16 = curFrame.localVars %= (// [(n-1, v)])
  | otherwise = setGlobalVar (n-16) v

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
parseTypeByte b = fmap (map fromJust) . mapM lookupType $ bitPairs b

parseTypeWord :: Word -> Emulator [ZType]
parseTypeWord w = let (b1, b2) = bytes w
                      bs = bitPairs b1 ++ bitPairs b2
                  in fmap (map fromJust) . mapM lookupType $ bs

bitPairs :: Byte -> [(Bool,Bool)]
bitPairs b' = takeWhile (/= (True, True)) [ (testBit b' 7, testBit b' 6)
                                          , (testBit b' 5, testBit b' 4)
                                          , (testBit b' 3, testBit b' 2)
                                          , (testBit b' 1, testBit b' 0)
                                          ]
