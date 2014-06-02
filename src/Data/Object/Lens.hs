{-# LANGUAGE TemplateHaskell, Rank2Types, FlexibleContexts #-}

module Data.Object.Lens where

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
import Safe
import Util
import Zscii
import Data.List (find)

data Property = Property { _num :: Byte
                         , _propData :: [Byte]
                         , _propAddr :: Int
                         }
  deriving (Show, Eq)

makeLenses ''Property

data Object = Object { _attributes :: [Bool]
                     , _parent :: Word
                     , _sibling :: Word
                     , _child :: Word
                     , _properties :: [Property]
                     , _address :: Int
                     }
  deriving (Show)

makeLenses ''Object

propertyDefaults :: Emulator' [Property]
propertyDefaults = do oaddr <- objectTableAddr
                      withTmpPC oaddr $ do forM [0..62] $ \i -> do
                                           w <- consumeWord
                                           let (b1, b2) = bytes w
                                           return $ Property (i+1) [b1,b2] 0

property :: Object -> Int -> Maybe Property
property obj ix = (obj^.properties) `atMay` ix

propertyIndex :: Object -> Int -> Maybe Int
propertyIndex obj propNum = findIndex ((==propNum) . fromIntegral . view num) (obj^.properties)

object :: Word -> Lens' EmuState Object
object w = lens getter setter
  where getter = evalState (objectAddr w >>= parseObjectAt)
        setter emu obj = execState (setObject w obj) emu

setObject :: Word -> Object -> Emulator' ()
setObject w obj' = do oaddr <- objectAddr w
                      writeObject obj' oaddr

propertyList :: Int -> Emulator' [Property]
propertyList paddr = withTmpPC paddr $ do
  consumePropertiesHeader
  propertyList'

propertyList' :: Emulator' [Property]
propertyList' = do p <- consumeProperty
                   case p of
                     Nothing -> return []
                     Just p' -> do rest <- propertyList'
                                   return (p':rest)

consumePropertiesHeader :: Emulator' [Word]
consumePropertiesHeader = do textLength <- consumeByte
                             replicateM (fromIntegral textLength) consumeWord

consumeProperty :: Emulator' (Maybe Property)
consumeProperty = do addr <- use thePC
                     sizeByte <- consumeByte
                     (propNum, len) <- if testBit sizeByte 7
                                        then twoByteSize sizeByte
                                        else return $ oneByteSize sizeByte
                     if sizeByte == 0
                       then return Nothing
                       else do dat <- replicateM len consumeByte
                               return . Just $ Property propNum dat addr
  where oneByteSize b = let propNum = b .&. (bit 6 - 1)
                            propLen = if testBit b 6 then 2 else 1
                        in (propNum, propLen)
        twoByteSize b = do let propNum = b .&. (bit 6 - 1)
                           b' <- consumeByte
                           let propLen' = fromIntegral $ b' .&. (bit 6 - 1)
                               propLen = if propLen' == 0 then 64 else propLen'
                           return (propNum, propLen)

writeProperty :: Word -> Property -> Emulator' ()
writeProperty = undefined

objectTableAddr :: Emulator' Int
objectTableAddr = do tableStart <- use $ memory.to (wordAt 0x0a)
                     let defaultsLength = 63 * 2
                     return $ fromIntegral (tableStart + defaultsLength)

objectAddr :: Word -> Emulator' Int
objectAddr w = do start <- objectTableAddr
                  return $ start + fromIntegral (w-1) * 14

writeObject :: Object -> Int -> Emulator' ()
writeObject obj addr = withTmpPC addr $ do
  mapM_ putByte . toBytes . view attributes $ obj
  mapM_ (putWord . flip view obj) [parent,sibling,child]
  return ()

toBytes :: [Bool] -> [Byte]
toBytes [] = []
toBytes (a:b:c:d:e:f:g:h:xs) = toByte [h,g,f,e,d,c,b,a] : toBytes xs
  where toByte bs = sum . map toByte' $ zip bs [0..]
        toByte' (False,_) = 0
        toByte' (True,i) = bit i
toBytes _ = error "toBytes received list of Bools not divisible by 8"

parseObjectAt :: Int -> Emulator' Object
parseObjectAt addr = withTmpPC addr $ do
  attrBytes <- replicateM 6 consumeByte
  let attrBits = concatMap toBits attrBytes
  p <- consumeWord
  s <- consumeWord
  c <- consumeWord
  paddr <- consumeWord
  props <- propertyList (fromIntegral paddr)
  return $ Object attrBits
                  p
                  s
                  c
                  props
                  addr
  where toBits b = map (testBit b) [7,6..0]
