module Test where

import Control.Applicative
import Control.Lens
import Data.Memory (Byte,Word,bytes,word,fromByteString)
import Emulator
import Test.QuickCheck
import Text.Printf
import qualified Data.ByteString as B

main :: IO ()
main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

prop_bytes_is_inverse_of_word :: (Byte, Byte) -> Bool
prop_bytes_is_inverse_of_word pair = (bytes . uncurry word) pair == pair

prop_word_is_inverse_of_bytes :: Word -> Bool
prop_word_is_inverse_of_bytes w = (uncurry word . bytes) w == w

infix 4 <=>
(<=>) :: (Eq a, Applicative f) => f a -> f a -> f Bool
(<=>) = liftA2 (==)

prop_load_pc_is_set :: Gen Bool
prop_load_pc_is_set = do n <- choose (0,2^16) :: Gen Int
                         w <- choose (0,fromIntegral n) :: Gen Word
                         let (b1,b2) = bytes w
                             bstr = B.concat [B.replicate 6 0, B.singleton b1, B.singleton b2, B.replicate n 0]
                             emu = load bstr
                         return $ emu^.thePC == fromIntegral w

tests :: [(String, IO ())]
tests = [ (
            "`bytes` is inverse of `word`",
            quickCheck (bytes . uncurry word <=> id)
          )

        , (
            "`word` is inverse of `bytes`",
            quickCheck (uncurry word . bytes <=> id)
          )

        , (
            "`load` PC starts at word at 0x6",
            quickCheck prop_load_pc_is_set
          )
        ]
