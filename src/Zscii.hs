module Zscii (decode) where

import Control.Arrow
import Control.Applicative
import Control.Monad.State
import Data.Maybe
import Data.Word
import Data.Bits
import Safe
import Data.Memory
import Debug.Trace

data Alphabet = AL | AU | AP
  deriving (Show, Enum, Eq)

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
alphaChange a i = toEnum $ (fromEnum a + i) `mod` 3

maskWord :: Word16 -> [Int]
maskWord w = [mask 1 5 w, mask 6 5 w, mask 11 5 w]

type ZSCII = State (Alphabet, [Int])

pull :: ZSCII Int
pull = gets (head . snd) <* modify (id *** tail)

decodeInt :: Int -> Alphabet -> ZSCII (Maybe Char)
decodeInt n al
  | n `elem` [4,5]     = changeAlphabet n
  | n == 6 && al == AP = do a <- pull
                            b <- pull
                            modify (const AL *** id)
                            return . Just . toEnum $ (a `shiftL` 5) + b
  | otherwise          = lookupChar n al

lookupChar w al = modify (const AL *** id) >> return (charLookup al w)
changeAlphabet w = modify (flip alphaChange w *** id) >> return Nothing

decodeState :: ZSCII [Maybe Char]
decodeState = do done <- gets (null . snd)
                 if done
                   then return []
                   else do al <- gets fst
                           n  <- pull
                           c  <- decodeInt n al
                           (:) <$> pure c <*> decodeState

decode :: [Word16] -> String
decode ws = let ints = concatMap maskWord ws
            in catMaybes $ evalState decodeState (AL, ints)
