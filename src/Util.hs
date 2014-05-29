module Util ( showHex
            , showBin
            , signedWord
            , signAtBit
            ) where

import qualified Numeric as N
import Data.Char (intToDigit)
import Data.Memory (Byte, Word)
import Data.Bits

-- Junk drawer time!

showHex :: (Integral a) => a -> String
showHex n = '$' : N.showHex (fromIntegral n :: Integer) ""

showBin :: Byte -> String
showBin n = '%' : padTo 8 '0' (N.showIntAtBase 2 intToDigit n "")
  where padTo k c str = replicate (k - length str) c ++ str

signedWord :: Word -> Int
signedWord = signAtBit 15

signAtBit :: (Integral a, Bits a) => Int -> a -> Int
signAtBit n b = let b' = fromIntegral $ b .&. (bit (n+1) - 1)
                in fromIntegral $ if testBit b n
                                    then b' - 2^(n+1)
                                    else b'
