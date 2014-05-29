module Instruction (exec) where

import Control.Monad.State
import Control.Monad
import Control.Lens
import Data.Memory
import Data.Maybe
import Data.Object
import Emulator
import Util
import qualified Debug as D
import Data.Bits
import Data.ZTypes
import Data.CallStack
import Zscii

exec :: Byte -> Emulator ()
exec b = do
  case (testBit b 7, testBit b 6) of
    (False,    _) -> exec2OP     b
    (True, False) -> execShortOP b
    (True,  True) -> execVAROP   b

exec2OP :: Byte -> Emulator ()
exec2OP b = do let opcode = b .&. (bit 5 - 1)
                   varType1 = testBit b 6
                   varType2 = testBit b 5
               x <- lookupAbbrevType varType1
               y <- lookupAbbrevType varType2
               D.log $ "Executing 2OP:" ++ showHex opcode ++ " with args " ++ show (x,y)
               do2OP opcode x y
  where lookupAbbrevType p  = if p
          then fmap fromJust . lookupType $ (True, False)
          else fmap fromJust . lookupType $ (False, True)

do2OP opcode x y = case opcode of
  0x1 {-je-} -> getLabel >>= jumpWith (readType x == readType y)
  0x2 {-jl-} -> getLabel >>= jumpWith (readType x < readType y)
  0x3 {-jg-} -> getLabel >>= jumpWith (readType x > readType y)
  0xd {-store-} -> setVarType x y
  0x11 {-get_prop-} -> do obj <- object (readType x)
                          Just p <- propertyWord obj (fromIntegral $ readType y)
                          setResult p
  0x14 {-add-} -> setResult (readType x + readType y)
  _ -> error $ "Got unknown 2OP:" ++ showHex opcode ++ " with args " ++ show x ++ ", " ++ show y

execShortOP :: Byte -> Emulator ()
execShortOP b = do
  let opcode = b .&. (bit 4 - 1)
  t <- lookupType (testBit b 5, testBit b 4)
  maybe (exec0OP opcode) (exec1OP opcode) t

getZString = do ws <- getStringWords
                return $ decode ws
  where getStringWords = do w <- consumeWord
                            if testBit w 15
                              then return [w]
                              else do ws <- getStringWords
                                      return (w:ws)

exec0OP opcode = D.log ("Executing 0OP:" ++ showHex opcode) >> case opcode of
  0x0 {-rtrue-} -> returnWith 1
  0x2 {-print-} -> getZString >>= liftIO . putStr
  0x8 {-ret_popped-} -> getVar 0 >>= returnWith
  0xa {-quit-} -> quit .= True
  0xb {-new_line-} -> liftIO $ putStr "\n"
  _ -> error $ "Got unknown 0OP:" ++ showHex opcode

getLabel :: Emulator (Int, Bool)
getLabel = do b <- consumeByte
              let backwards = not $ testBit b 7
                  done = testBit b 6
              if done
                then return (fromIntegral $ b .&. (bit 5 - 1), backwards)
                else do b' <- consumeByte
                        let w = word b b'
                        return (signAtBit 13 w, backwards)


jumpWith :: Bool -> (Int, Bool) -> Emulator ()
jumpWith predicate (label, backwards) = do let shouldJump = if backwards
                                                              then not predicate
                                                              else predicate
                                           case label of
                                             0 -> returnWith 0
                                             1 -> returnWith 1
                                             _ -> when shouldJump $ do p <- use thePC
                                                                       D.log $ "PC is " ++ showHex p
                                                                       D.log $ "Attempting jump to " ++ showHex (p + label - 2)
                                                                       thePC += label - 2

exec1OP :: Byte -> ZType -> Emulator ()
exec1OP opcode t = D.log ("Executing 1OP:" ++ showHex opcode ++ " with arg " ++ show t) >> case opcode of
  0x0 {-jz-} -> do let val = readType t
                   getLabel >>= jumpWith (val == 0)
  0xc {-jump-} -> do let (ZWord label) = t
                     p <- use thePC
                     D.log $ "PC is " ++ showHex p
                     D.log $ "Jumping to " ++ show (signedWord label)
                     thePC += signedWord label - 2
  0xd {-print_paddr-} -> do let ZWord packed = t
                                stringAddr = unpackAddress packed
                            origPC <- use thePC
                            thePC .= stringAddr
                            getZString >>= liftIO . putStr
                            thePC .= origPC
  _ -> error $ "Got unknown 1OP:" ++ showHex opcode ++ " with argument " ++ show t

execVAROP :: Byte -> Emulator ()
execVAROP b = do
  let opcode = b .&. (bit 5 - 1)
      isVAR  = testBit b 5
  args <- parseTypeByte =<< consumeByte
  if isVAR
    then do
      D.log ("Executing VAROP:" ++ showHex opcode ++ " with args " ++ show args)
      doVAROP opcode args
    else case args of
           [x,y] -> do2OP opcode x y
           _ -> error "Wrong number of arguments supplied to 2OP"

doVAROP opcode args = case opcode of
  0x0 {-call_vs-} -> do let values = map readType args
                        res <- callRoutine (head values) (tail values)
                        setResult res
  0x6 {-print_num-} -> do let val = head args
                          liftIO . putStr . show $ readType val
  0x19 {-call_vn-} -> do let values = map readType args
                         _ <- callRoutine (head values) (tail values)
                         return ()
  _ -> error $ "Got unknown VAROP " ++ showHex opcode ++ " with arguments " ++ show args

callRoutine :: Word -> [Word] -> Emulator Word
callRoutine routine args = do
  let raddr = unpackAddress routine
  numLocals <- fmap fromIntegral (peekByteAt raddr)
  let locals = take numLocals $ args ++ repeat 0
      newFrame = newStackFrame (raddr + 1) locals
  D.log $ "Executing routine at " ++ showHex raddr ++ " with " ++ show args
  callStack %= push newFrame
  execLoop
  ret <- use $ curFrame.returnValue.to fromJust
  callStack %= discard
  return ret
  where execLoop = do instr <- consumeByte
                      exec instr
                      shouldReturn <- use $ curFrame.returnValue.to isJust
                      hasQuit <- use quit
                      when (not $ shouldReturn || hasQuit) execLoop

returnWith :: Word -> Emulator ()
returnWith v = do D.log $ "Returning " ++ showHex v
                  curFrame.returnValue .= Just v
