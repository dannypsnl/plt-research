module basic

import Data.List

mutual
  isEven : Nat -> Bool
  isEven Z = True
  isEven (S k) = isOdd k

  isOdd : Nat -> Bool
  isOdd Z = False
  isOdd (S k) = isEven k

myReverse : List a -> List a
myReverse [] = []
myReverse (x :: xs) = (myReverse xs) ++ [x]
