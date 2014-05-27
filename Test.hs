module Test where

import Test.QuickCheck
import Text.Printf
import Data.Memory (Byte,Word,bytes,word)

main :: IO ()
main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

prop_bytes_is_inverse_of_word :: (Byte, Byte) -> Bool
prop_bytes_is_inverse_of_word pair = (bytes . uncurry word) pair == pair

prop_word_is_inverse_of_bytes :: Word -> Bool
prop_word_is_inverse_of_bytes w = (uncurry word . bytes) w == w

tests :: [(String, IO ())]
tests = [ ("`bytes` is inverse of `word`", quickCheck prop_bytes_is_inverse_of_word)
        , ("`word` is inverse of `bytes`", quickCheck prop_word_is_inverse_of_bytes)
        ]
