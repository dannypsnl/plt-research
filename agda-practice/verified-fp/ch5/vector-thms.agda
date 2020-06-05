module vector-thms where

open import Data.Vec
open import Data.Nat

repeat𝕍 : ∀ {𝓁} {A : Set 𝓁} → (a : A) (n : ℕ) → Vec A n
repeat𝕍 a 0 = []
repeat𝕍 a (suc n) = a ∷ (repeat𝕍 a n)
