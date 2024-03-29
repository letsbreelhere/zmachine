{-# LANGUAGE TemplateHaskell, FlexibleContexts, RankNTypes #-}

module Instruction (exec) where

import Control.Monad.State
import Control.Monad
import Control.Lens
import Data.Memory
import Data.Maybe
import Data.Object.Lens
import Emulator
import Util
import Safe
import qualified Debug as D
import Data.Bits
import Data.ZTypes
import Data.CallStack
import Zscii

exec :: Byte -> Emulator ()
exec 0xbe = execEXTOP
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
               do2OP opcode x y
  where lookupAbbrevType p  = if p
          then fmap fromJust . lookupType $ (True, False)
          else fmap fromJust . lookupType $ (False, True)

do2OP opcode x y = do
  D.log $ "Executing 2OP:" ++ showHex opcode ++ " with args " ++ show (x,y)
  case opcode of
    0x1 {-je-} -> getLabel >>= jumpWith (readType x == readType y)
    0x2 {-jl-} -> getLabel >>= jumpWith (signedWord (readType x) < signedWord (readType y))
    0x3 {-jg-} -> getLabel >>= jumpWith (signedWord (readType x) > signedWord (readType y))
    0x4 {-dec_chk-} -> do curVar <- getVar (fromIntegral $ readType x)
                          setVar (fromIntegral $ readType x) (curVar - 1)
                          getLabel >>= jumpWith (curVar - 1 < readType y)
    0x5 {-inc_chk-} -> do curVar <- getVar (fromIntegral $ readType x)
                          setVar (fromIntegral $ readType x) (curVar + 1)
                          getLabel >>= jumpWith (curVar + 1 > readType y)
    0x6 {-jin-} -> do obj <- use $ object (readType x)
                      getLabel >>= jumpWith (obj^.parent == readType y)
    0x8 {-or-} -> doBitwise (.|.)
    0x9 {-and-} -> doBitwise (.&.)
    0xa {-test_attr-} -> do obj <- use $ object (readType x)
                            let attr = fromIntegral $ readType y
                            getLabel >>= jumpWith ((obj^.attributes) !! attr)
    0xb {-set_attr-} -> do let w = readType x
                           obj <- use $ object w
                           let attr = fromIntegral $ readType y
                               newAttrs = take attr (obj^.attributes) ++ [True] ++ drop (attr+1) (obj^.attributes)
                           (object w).attributes .= newAttrs
    0xc {-clear_attr-} -> do let w = readType x
                             obj <- use $ object w
                             let attr = fromIntegral $ readType y
                                 newAttrs = take attr (obj^.attributes) ++ [False] ++ drop (attr+1) (obj^.attributes)
                             (object w).attributes .= newAttrs
    0xd {-store-} -> setVarType x y
    0xf {-loadw-} -> do let array = readType x
                            wordIndex = 2 * readType y
                        peekWordAt (array + wordIndex) >>= setResult
    0x10 {-loadb-} -> do let array     = readType x
                             byteIndex = readType y
                         peekByteAt (array + byteIndex) >>= setResult . fromIntegral
    0x11 {-get_prop-} -> do let w = readType x
                            obj <- use $ object w
                            let ix = fromIntegral $ readType y
                                p' = obj `property` ix
                            defs <- propertyDefaults
                            let p = fromMaybe (defs !! ix) p'
                            case p^.propData of
                              [b]     -> setResult $ word 0 b
                              [b1,b2] -> setResult $ word b1 b2
                              _       -> error "Tried to set property of more than 2 bytes"
    0x12 {-get_prop_addr-} -> do obj <- use $ object (readType x)
                                 let p = obj `property` (fromIntegral $ readType y)
                                 setResult $ maybe 0 (fromIntegral . (^.propAddr)) p
    0x13 {-get_next_prop-} -> do obj <- use $ object (readType x)
                                 let propNum = fromIntegral $ readType y
                                 let ix = case propNum of
                                            0 -> Just 0
                                            _ -> fmap (+1) $ propertyIndex obj propNum
                                 let prop = ix >>= property obj
                                     res  = fmap (view num) prop
                                 setResult . fromIntegral . fromMaybe 0 $ res
    0x14 {-add-} -> doArith (+)
    0x15 {-sub-} -> doArith (-)
    0x16 {-mul-} -> doArith (*)
    0x17 {-div-} -> do let res = truncate $ (fromIntegral . signedWord . readType) x / (fromIntegral . signedWord . readType) y
                       setResult res
    0x18 {-mod-} -> do let quo = truncate $ (fromIntegral . signedWord . readType) x / (fromIntegral . signedWord . readType) y
                           s = signedWord . readType $ x
                           t = signedWord . readType $ y
                           res = fromIntegral $ s - t * quo
                       setResult res
    0x19 {-call_2s-} -> do res <- callRoutine (readType x) [readType y]
                           setResult res
    0x1a {-call_2n-} -> void $ callRoutine (readType x) [readType y]
    _ -> error $ "Got unknown 2OP:" ++ showHex opcode ++ " with args " ++ show x ++ ", " ++ show y
  where doArith f = setResult . fromIntegral $ signedWord (readType x) `f` signedWord (readType y)
        doBitwise f = setResult $ readType x `f` readType y

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
  0x1 {-rfalse-} -> returnWith 0
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
  0x1 {-get_sibling-} -> do obj <- use $ object (readType t)
                            setResult (obj^.sibling)
                            getLabel >>= jumpWith (obj^.sibling /= 0)
  0x2 {-get_sibling-} -> do obj <- use $ object (readType t)
                            setResult (obj^.child)
                            getLabel >>= jumpWith (obj^.child /= 0)
  0x3 {-get_parent-}  -> use (object $ readType t) >>= setResult . view parent
  0x4 {-get_prop_len-} -> do mp <- withTmpPC (fromIntegral $ readType t) $ consumeProperty
                             setResult $ case mp of
                               Nothing -> error "Use default property length here?"
                               Just p  -> fromIntegral . length $ p^.propData
  0x5 {-inc-} -> do let var = fromIntegral (readType t) :: Byte
                    v <- getVar var
                    setVar var (v + 1)
  0x6 {-dec-} -> do let var = fromIntegral (readType t) :: Byte
                    v <- getVar var
                    setVar var (v - 1)
  0x8 {-call_1s-} -> callRoutine (readType t) [] >>= setResult
  0xb {-ret-} -> returnWith (readType t)
  0xc {-jump-} -> do let (ZWord label) = t
                     p <- use thePC
                     D.log $ "PC is " ++ showHex p
                     D.log $ "Jumping to " ++ show (signedWord label)
                     thePC += signedWord label - 2
  0xd {-print_paddr-} -> do let stringAddr = unpackAddress (readType t)
                            origPC <- use thePC
                            thePC .= stringAddr
                            getZString >>= liftIO . putStr
                            thePC .= origPC
  0xe {-load-} -> getVar (fromIntegral $ readType t) >>= setResult
  0xf {-call_1n-} -> void $ callRoutine (readType t) []
  _ -> error $ "Got unknown 1OP:" ++ showHex opcode ++ " with argument " ++ show t

execVAROP :: Byte -> Emulator ()
execVAROP b = do
  let opcode = b .&. (bit 5 - 1)
      isVAR  = testBit b 5
  args <- getArgs opcode isVAR
  if isVAR
    then do
      D.log ("Executing VAROP:" ++ showHex opcode ++ " with args " ++ show args)
      doVAROP opcode args
    else case args of
           [x,y] -> do2OP opcode x y
           (x:ys) -> if opcode == 1 -- Wooo, special cases! `je`
                       then getLabel >>= jumpWith (readType x `elem` map readType ys)
                       else error $ "Wrong number of arguments supplied to 2OP:" ++ showHex opcode ++ " - " ++ show args
  where getArgs opcode isVAR
          | not isVAR = parseTypeByte =<< consumeByte
          | opcode == 0xc  = parseTypeWord =<< consumeWord
          | opcode == 0x1a = parseTypeWord =<< consumeWord
          | otherwise = parseTypeByte =<< consumeByte

doVAROP :: Byte -> [ZType] -> Emulator ()
doVAROP opcode args = case opcode of
  0x0 {-call_vs-} -> do let values = map readType args
                        res <- callRoutine (head values) (tail values)
                        setResult res
  0x1 {-storew-} -> do
    let (array':wordIndex':value':_) = args
        array = fromIntegral $ readType array'
        wordIndex = fromIntegral $ readType wordIndex'
        value = fromIntegral $ readType value'
    memory %= writeWord (array+2*wordIndex) value
  0x2 {-storeb-} -> do
    let (array':byteIndex':value':_) = args
        array = fromIntegral $ readType array'
        byteIndex = fromIntegral $ readType byteIndex'
        value = fromIntegral $ readType value'
    memory %= writeByte (array+byteIndex) value
  0x3 {-put_prop-} -> do let (o:prop:value:_) = map readType args
                         error "put_prop not implemented"
  0x6 {-print_num-} -> do let val = head args
                          liftIO . putStr . show . signedWord $ readType val
  0x8 {-push-} -> setVar 0 (readType $ head args)
  0x9 {-pull-} -> getVar 0 >>= setVar (fromIntegral . readType . head $ args)
  0xc {-call_vs2-} -> do let values = map readType args
                         callRoutine (head values) (tail values) >>= setResult
  0x18 {-not-} -> setResult $ complement (readType $ head args)
  0x19 {-call_vn-} -> do let values = map readType args
                         _ <- callRoutine (head values) (tail values)
                         return ()
  0x1a {-call_vn2-} -> do let values = map readType args
                          _ <- callRoutine (head values) (tail values)
                          return ()
  0x1f {-check_arg_count-} -> do
    let expectedArgs = readType (head args)
    actualArgs <- use $ curFrame.argCount
    getLabel >>= jumpWith (fromIntegral expectedArgs == actualArgs)
  _ -> error $ "Got unknown VAROP " ++ showHex opcode ++ " with arguments " ++ show args

callRoutine :: Word -> [Word] -> Emulator Word
callRoutine routine args = do
  let raddr = unpackAddress routine
  numLocals <- fmap fromIntegral (peekByteAt raddr)
  let locals = take numLocals $ args ++ repeat 0
      newFrame = newStackFrame (raddr + 1) locals (length args)
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

execEXTOP = do
  b <- consumeByte
  args <- parseTypeByte =<< consumeByte
  D.log $ "Executing EXTOP:" ++ showHex b
  doEXTOP b args

doEXTOP opcode args = case opcode of
  0x2 {-log_shift-} -> do let [number', places'] = args
                              number = readType number'
                              places = signedWord (readType places')
                          setResult . fromIntegral $ number `shift` places
  0x3 {-art_shift-} -> do let [number', places'] = args
                              number = signedWord (readType number')
                              places = signedWord (readType places')
                          setResult . fromIntegral $ number `shift` places
  _ -> error $ "Got unknown EXTOP " ++ showHex opcode ++ " with arguments " ++ show args

returnWith :: Word -> Emulator ()
returnWith v = do D.log $ "Returning " ++ showHex v
                  curFrame.returnValue .= Just v
