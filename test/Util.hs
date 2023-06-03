module Util (fromRight) where

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight (Left _) = error "Called fromRight on Left"