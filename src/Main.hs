module Main where

import Control.Lens
import Emulator
import Emulator.Run
import System.Environment
import Control.Applicative
import Util
import qualified Data.ByteString as B

main :: IO ()
main = do
  file:_ <- getArgs
  initState' <- load <$> B.readFile file
  let initState = initState' & options.debug .~ False
  _ <- runEmulator initState
  return ()
