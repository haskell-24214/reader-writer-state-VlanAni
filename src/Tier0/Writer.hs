module Tier0.Writer (Tree (..), sumAndTraceInOrder) where

import Control.Monad.Writer

data Tree a = Leaf a | Branch (Tree a) a (Tree a) deriving Eq

sumAndTraceInOrder :: Num a => Tree a -> Writer [a] a
sumAndTraceInOrder (Leaf a) = do 
    tell [a]
    return a 
sumAndTraceInOrder (Branch left a right) = do 
    leftSum <- sumAndTraceInOrder left 
    let curr = a 
    tell [a]
    rightSum <- sumAndTraceInOrder right 
    let totalSum = leftSum + curr + rightSum 
    return totalSum 
