module Util (fromRight, isRight) where

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight (Left _) = error "Called fromRight on Left"

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False