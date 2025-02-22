module Tier2.Writer (collectAndSumInOrder) where

import Control.Monad.Writer
import Tier0.Writer (Tree (..))

collectAndSumInOrder :: Num a => Tree a -> Writer (Sum a) [a]
collectAndSumInOrder (Leaf a) = do
    tell (Sum a) 
    return (a:[])
collectAndSumInOrder (Branch left a right) = do 
    left_vert <- collectAndSumInOrder left 
    tell (Sum a) 
    right_vert <- collectAndSumInOrder right 
    return (left_vert ++ (a:[]) ++ right_vert)  

