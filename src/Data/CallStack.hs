{-# LANGUAGE TemplateHaskell #-}

module Data.CallStack ( StackFrame
                      , pc
                      , localVars
                      , returnValue
                      , localStack
                      , newStackFrame
                      , CallStack
                      , top
                      , bottom
                      , push
                      , argCount
                      , discard
                      , NEList(..)
                      ) where

import Data.Array
import Control.Lens
import Data.Word (Word8,Word16)

type Byte = Word8
type Word = Word16

data NEList a = a :# [a]

top :: Lens' (NEList a) a
top = let getter (x :#  _)    = x
          setter (_ :# xs) x' = x' :# xs
      in  lens getter setter

bottom :: Lens' (NEList a) [a]
bottom = let getter (_ :# xs)     = xs
             setter (x :#  _) xs' = x :# xs'
         in  lens getter setter

push :: a -> NEList a -> NEList a
push x (y :# zs) = x :# (y:zs)

discard :: NEList a -> NEList a
discard (_ :# []) = error "Tried to make an empty NEList!"
discard (_ :# (sf:sfs)) = sf :# sfs

data StackFrame = StackFrame { _pc :: Int
                             , _localVars :: Array Byte Word
                             , _localStack :: [Word]
                             , _returnValue :: Maybe Word
                             , _argCount :: Int
                             }

makeLenses ''StackFrame

newStackFrame :: Int -> [Word] -> Int -> StackFrame
newStackFrame pc xs n = StackFrame pc
                                   (listArray (0, fromIntegral $ length xs) xs)
                                   []
                                   Nothing
                                   0

type CallStack = NEList StackFrame
