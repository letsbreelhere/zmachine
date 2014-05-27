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
top = let getter (x :# _)     = x
          setter (_ :# xs) x' = x' :# xs
      in  lens getter setter

bottom :: Lens' (NEList a) [a]
bottom = let getter (_ :# xs)    = xs
             setter (x :# _) xs' = x :# xs'
         in  lens getter setter

data StackFrame = StackFrame { _pc :: Int
                             }

makeLenses ''StackFrame

newStackFrame :: Int -> StackFrame
newStackFrame = StackFrame

data CallStack = CallStack (NEList StackFrame)

discard :: CallStack -> CallStack
discard (CallStack (_ :# [])) = error "Tried to make call stack empty!"
discard (CallStack (_ :# (sf:sfs))) = CallStack (sf :# sfs)
