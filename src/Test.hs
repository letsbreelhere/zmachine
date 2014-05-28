module Test where

import Test.HUnit

import Control.Lens
import Emulator
import Emulator.Run
import qualified Data.ByteString as B

testBlank = TestCase $ do
  initState <- fmap load (B.readFile "../games/blank.z5")
  final <- runEmulator initState
  let quitByte = 0x4f6
  assertEqual "PC stops at quit instruction" quitByte (final^.thePC)

tests = TestList [ testBlank
                 ]

main = runTestTT tests
