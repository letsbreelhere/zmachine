{-# LANGUAGE TemplateHaskell #-}

module Data.Object where

import qualified Debug as D
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Lens
import Emulator
import Data.List
import Data.Maybe
import Data.Memory
import Data.Bits
import Util
import Zscii
import Data.List (find)

data Object = Object { _attributes :: [Bool]
                     , _parent :: Word
                     , _sibling :: Word
                     , _child :: Word
                     , _properties :: Int
                     , _address :: Int
                     }
  deriving (Show)

makeLenses ''Object

data Property = Property { _num :: Byte
                         , _propData :: [Byte]
                         , _propAddr :: Int
                         }
  deriving (Show, Eq)

makeLenses ''Property

toZString :: Property -> String
toZString p = decode (pairsToWords $ p^.propData)
  where pairsToWords xs
          | length xs `mod` 2 == 1 = error "Tried to convert odd-numbered bytestring to ZString!"
          | otherwise = case xs of
              [] -> []
              b1:b2:bs -> word b1 b2 : pairsToWords bs

objectTableAddr :: Emulator Int
objectTableAddr = do tableStart <- fmap fromIntegral . use $ memory.to (wordAt 0x0a)
                     let defaultsLength = 63 * 2
                     return $ tableStart + defaultsLength

-- "Objects are numbered consecutively from 1 upward, with object number 0 being
-- used to mean "nothing" (though there is formally no such object)."

objectAddr :: Word -> Emulator Int
objectAddr w = do start <- objectTableAddr
                  return $ start + fromIntegral (w-1) * 14

object :: Word -> Emulator Object
object w = objectAddr w >>= parseObjectAt

propertyDefaults :: Emulator [Property]
propertyDefaults = do tableStart <- fmap fromIntegral . use $ memory.to (wordAt 0x0a)
                      forM [0..62] $ \i -> do
                        w <- consumeWord
                        let (b1, b2) = bytes w
                        return $ Property (i+1) [b1,b2] 0

{-
Each object has a 14-byte entry as follows:

    the 48 attribute flags     parent    sibling   child     properties
   ---48 bits in 6 bytes---   ---3 words, i.e. 6 bytes----  ---2 bytes--
-}

parseObjectAt :: Int -> Emulator Object
parseObjectAt addr = withTmpPC addr $ do
  attrBytes <- replicateM 6 consumeByte
  let attrBits = concatMap toBits attrBytes
  p <- consumeWord
  s <- consumeWord
  c <- consumeWord
  props <- consumeWord
  return $ Object attrBits
                  p
                  s
                  c
                  (fromIntegral props)
                  addr
  where toBits b = map (testBit b) [7,6..0]

getAttr :: Object -> Int -> Bool
getAttr obj attr
  | attr >= 48 = error $ "Tried to get an attr past 48: " ++ show attr
  | otherwise  = obj^.attributes.to (!! attr)

setAttr :: Object -> Int -> Emulator ()
setAttr obj attr
  | attr >= 48 = error $ "Tried to set an attr past 48: " ++ show attr
  | otherwise = do oldPC <- use thePC
                   let attrAddress = obj^.address + (attr `div` 8)
                   b <- peekByteAt attrAddress
                   let newByte = setBit b (7 - attr `mod` 8)
                   memory %= writeByte attrAddress newByte
                   thePC .= oldPC

clearAttr :: Object -> Int -> Emulator ()
clearAttr obj attr
  | attr >= 48 = error $ "Tried to set an attr past 48: " ++ show attr
  | otherwise = do oldPC <- use thePC
                   let attrAddress = obj^.address + (attr `div` 8)
                   b <- peekByteAt attrAddress
                   let newByte = clearBit b (7 - attr `mod` 8)
                   memory %= writeByte attrAddress newByte
                   thePC .= oldPC

propertyList :: Object -> Emulator [Property]
propertyList obj = withTmpPC (fromIntegral $ obj^.properties) $ do
  consumePropertiesHeader
  propertyList'
  where propertyList' = do p <- consumeProperty
                           case p of
                             Nothing -> return []
                             Just p' -> (:) <$> pure p' <*> propertyList'

property :: Object -> Int -> Emulator (Maybe Property)
property obj i = do ps <- propertyList obj
                    return $ find (\p' -> p'^.num == fromIntegral i) ps

propertyIndex :: Object -> Int -> Emulator Int
propertyIndex obj i = do mprop <- property obj i
                         ps    <- propertyList obj
                         let ix = mprop >>= \prop -> elemIndex prop ps
                         return $ fromMaybe 0 ix

propertyWord :: Object -> Int -> Emulator (Maybe Word)
propertyWord obj i = do mp <- property obj i
                        let p = fmap (^.propData) mp
                        return $ fmap toPropWord p
  where toPropWord xs = case xs of
          [x]   -> word 0 x
          [x,y] -> word x y
          _     -> error "Improper number of args passed for get_prop!"

propertyName :: Object -> Emulator String
propertyName obj = withTmpPC (fromIntegral $ obj^.properties) $ do
  fmap decode consumePropertiesHeader

consumePropertiesHeader :: Emulator [Word]
consumePropertiesHeader = do textLength <- fmap fromIntegral consumeByte
                             replicateM textLength consumeWord

consumeProperty :: Emulator (Maybe Property)
consumeProperty = do addr <- use thePC
                     sizeByte <- consumeByte
                     (propNum, len) <- if testBit sizeByte 7
                                        then twoByteSize sizeByte
                                        else return $ oneByteSize sizeByte
                     if sizeByte == 0
                       then return Nothing
                       else fmap Just $ Property <$> pure propNum <*> replicateM len consumeByte <*> pure addr
  where oneByteSize b = let propNum = b .&. (bit 6 - 1)
                            propLen = if testBit b 6 then 2 else 1
                        in (propNum, propLen)
        twoByteSize b = do let propNum = b .&. (bit 6 - 1)
                           b' <- consumeByte
                           let propLen' = fromIntegral $ b' .&. (bit 6 - 1)
                               propLen = if propLen' == 0 then 64 else propLen'
                           return (propNum, propLen)
