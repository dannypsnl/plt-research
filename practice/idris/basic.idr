module basic

import Data.List
import Data.Vect

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

insert : Ord elem => elem -> Vect n elem -> Vect (S n) elem
insert e [] = [e]
insert e (x :: xs) = if x > e then e :: x :: xs
                              else x :: insert e xs

insSort : Ord elem => Vect n elem -> Vect n elem
insSort [] = []
insSort (x :: xs) = let sortedXs = insSort xs in
                        insert x sortedXs
