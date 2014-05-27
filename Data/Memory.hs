module Data.Memory ( Byte
                   , Word
                   , Memory
                   , writeByte
                   , writeWord
                   , byteAt
                   , wordAt
                   , fromByteString
                   ) where

import Data.Array
import Data.Word (Word8, Word16)
import Data.Bits
import qualified Data.ByteString as B

type Byte = Word8
type Word = Word16

data Memory = Memory { unwrapMem :: Array Int Word8 }

bytes :: Word -> (Byte, Byte)
bytes w = (fromIntegral (w `shiftR` 8), fromIntegral (w .&. 0xff))

word :: Byte -> Byte -> Word
word b1 b2 = 0x100 * fromIntegral b1 + fromIntegral b2

writeByte :: Int -> Byte -> Memory -> Memory
writeByte ix b m = Memory $ unwrapMem m // [(ix,b)]

writeWord :: Int -> Word -> Memory -> Memory
writeWord ix w m = let (b1,b2) = bytes w
                   in Memory $ unwrapMem m // [(ix,b1),(ix+1,b2)]

byteAt :: Int -> Memory -> Byte
byteAt ix m = unwrapMem m ! ix

wordAt :: Int -> Memory -> Word
wordAt ix m = let b1 = byteAt ix m
                  b2 = byteAt (ix+1) m
              in word b1 b2

fromByteString :: B.ByteString -> Memory
fromByteString bstr = let bs = B.unpack bstr
                      in Memory $ listArray (0, length bs) bs
