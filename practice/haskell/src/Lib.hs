module Lib where

import Debug.Trace
import Control.Monad.State

list1 :: [Int]
list1 = [2 * x | x <- [1, 2, 3, 4, 5]]
list2 :: [Int]
list2 = [n * x | x <- [1, 2, 3, 4, 5], let n = 3]
list3 :: [Int]
list3 = [n * x | x <- [1, 2, 3, 4, 5], let n = 3, odd x]

traceInt :: Int
traceInt = trace "tracing" 1

test :: State Int Int
test = do
  modify (+1)
  get
int1 :: Int
int1 = execState test 0
