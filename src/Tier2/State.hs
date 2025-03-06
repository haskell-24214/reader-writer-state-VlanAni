module Tier2.State where

import Control.Monad.State

data Registers = Registers { ax :: Int, bx :: Int, blink :: Bool, acc :: Int }

emptyRegisters = Registers 0 0 False 0

type Calculation = State Registers Int

plus :: Calculation
plus = do
  currRegs <- get 
  let ax_now = ax currRegs
      bx_now = bx currRegs 
      acc_now = bx_now + ax_now 
  put currRegs { acc = acc_now, blink = False } 
  return acc_now

minus :: Calculation
minus = do
  currRegs <- get 
  let ax_now = ax currRegs
      bx_now = bx currRegs 
      acc_now = ax_now - bx_now 
  put currRegs { acc = acc_now, blink = False } 
  return acc_now


productS :: Calculation
productS = do
  currRegs <- get 
  let ax_now = ax currRegs
      bx_now = bx currRegs 
      acc_now = bx_now * ax_now 
  put currRegs { acc = acc_now, blink = False } 
  return acc_now


divit :: Calculation
divit = do
  currRegs <- get 
  let ax_now = ax currRegs
      bx_now = bx currRegs 
  if (bx_now == 0) 
    then do  
      put currRegs { ax = 0, bx = 0, acc = 0, blink = False } 
      return 0 
    else do 
      let acc_now = ax_now `div` bx_now 
      put currRegs { acc = acc_now, blink = False }
      return acc_now


swap :: Calculation
swap = do
  currRegs <- get 
  let ax_now = ax currRegs 
      bx_now = bx currRegs 
      acc_now = acc currRegs
  put currRegs { ax = bx_now, bx = ax_now }
  return acc_now

blinkS :: Calculation
blinkS = do 
  currRegs <- get 
  let blink_now = blink currRegs 
      blink_new = not blink_now  
      acc_now = acc currRegs
  put currRegs { blink = blink_new }
  return acc_now

accS :: Calculation
accS = do 
  currRegs <- get
  let acc_now = acc currRegs 
      blink_now = blink currRegs
  if blink_now
    then do 
      put currRegs { bx = acc_now, blink = False }
    else do 
      put currRegs { ax = acc_now, blink = True }
  return acc_now
  
number :: Int -> Calculation
number x = do
  currRegs <- get
  let blink_now = blink currRegs 
      acc_now = acc currRegs
  if blink_now
    then do 
      put currRegs { bx = x, blink = False }
    else do 
      put currRegs { ax = x, blink = True }  
  return acc_now

commandToCalculation :: String -> Calculation
commandToCalculation s =
  case s of
    "+" -> plus
    "-" -> minus
    "/" -> divit 
    "*" -> productS
    "swap" -> swap
    "blink" -> blinkS
    "acc" -> accS
    x -> number (read x)

buildCalculation :: [String] -> Calculation
buildCalculation xs = 
  foldl (\a x -> a >>= (\_ -> x)) (state (\s -> (0, s))) (map commandToCalculation xs)

calculate :: [String] -> Int
calculate xs = evalState (buildCalculation xs) emptyRegisters
