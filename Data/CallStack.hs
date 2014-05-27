{-# LANGUAGE TemplateHaskell #-}

module Data.CallStack ( StackFrame
                      , pc
                      , newStackFrame
                      , CallStack
                      , top
                      , bottom
                      , discard
                      ) where

import Control.Lens

data NEList a = a :# [a]

top :: Lens' (NEList a) a
top = let getter (x :#  _)    = x
          setter (_ :# xs) x' = x' :# xs
      in  lens getter setter

bottom :: Lens' (NEList a) [a]
bottom = let getter (_ :# xs)     = xs
             setter (x :#  _) xs' = x :# xs'
         in  lens getter setter

discard :: NEList a -> NEList a
discard (_ :# []) = error "Tried to make an empty NEList!"
discard (_ :# (sf:sfs)) = sf :# sfs

data StackFrame = StackFrame { _pc :: Int
                             }

makeLenses ''StackFrame

newStackFrame :: Int -> StackFrame
newStackFrame = StackFrame

type CallStack = NEList StackFrame
