module Util ( showHex
            , showBin
            ) where

import qualified Numeric as N
import Data.Char (intToDigit)
import Data.Memory (Byte)

-- Junk drawer time!

showHex :: (Integral a) => a -> String
showHex n = '$' : N.showHex (fromIntegral n :: Integer) ""

showBin :: Byte -> String
showBin n = '%' : padTo 8 '0' (N.showIntAtBase 2 intToDigit n "")
  where padTo k c str = replicate (k - length str) c ++ str
