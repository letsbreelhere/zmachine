module Zscii (decode) where

import Control.Applicative
import Control.Monad.State
import Data.Maybe
import Data.Word
import Data.Bits
import Safe

data Alphabet = AL | AU | AP
  deriving (Show, Enum)

-- Mask 16-bit word w, starting at index s, with length l
-- E.g.: mask 2 5   1101010101011100
--                & 0011111000000000
--                  -----------
--                =   01010

mask :: Int -> Int -> Word16 -> Int
mask s l w = let masker = (2^l - 1)          `shiftL` (16 - s - l)
             in  (fromIntegral w .&. masker) `shiftR` (16 - s - l)

charLookup :: Alphabet -> Int -> Maybe Char
charLookup _ 0 = Just ' '
charLookup alphabet i = table `atMay` (i-6)
  where table = case alphabet of
                  AL -> ['a'..'z']
                  AU -> ['A'..'Z']
                  AP -> "^\n0123456789.,!?_#'\"/\\-:()"

alphaChange :: Alphabet -> Int -> Alphabet
alphaChange a i = toEnum (fromEnum a + i `mod` 3)

maskWord :: Word16 -> [Int]
maskWord w = [mask 1 5 w, mask 6 5 w, mask 11 5 w]

decodeInt :: Int -> State Alphabet (Maybe Char)
decodeInt w = do al <- get
                 if w `elem` [4,5]
                   then put (alphaChange al w) >> return Nothing
                   else put AL >> return (charLookup al w)

decodeState :: [Int] -> State Alphabet String
decodeState [] = return ""
decodeState (i:is) = do mc <- decodeInt i
                        case mc of
                          Nothing -> decodeState is
                          Just c -> (:) <$> pure c <*> decodeState is

decode :: [Word16] -> String
decode ws = let ints = concatMap maskWord ws
            in evalState (decodeState ints) AL
