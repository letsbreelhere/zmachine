{-# LANGUAGE TemplateHaskell #-}

module Data.CallStack ( StackFrame
                      , pc
                      , returnValue
                      , newStackFrame
                      , CallStack
                      , top
                      , bottom
                      , push
                      , discard
                      , NEList(..)
                      ) where

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
                             , _returnValue :: Maybe Word
                             }

makeLenses ''StackFrame

newStackFrame :: Int -> StackFrame
newStackFrame pc = StackFrame pc Nothing

type CallStack = NEList StackFrame
