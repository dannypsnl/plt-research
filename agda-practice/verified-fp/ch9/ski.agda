module ski where

open import Data.Nat

data comb : Set where
  S : comb
  K : comb
  app : comb → comb → comb

size : comb → ℕ
size S = 1
size K = 1
size (app n m) = suc ((size n) + (size m))
