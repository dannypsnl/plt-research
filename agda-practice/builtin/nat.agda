module nat where

open import Data.Nat

data MyNat : Set where
  zero : MyNat
  succ : MyNat -> MyNat
  
_my+_ : MyNat → MyNat → MyNat
zero my+ m = m
succ n my+ m = succ (n my+ m)
