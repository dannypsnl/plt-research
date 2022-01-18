module game where

open import Data.Nat
open import Agda.Builtin.Sigma

Type = Set 

data ⊥ : Type where
data ⊤ : Type where
  tt : ⊤

true : ⊤
true = tt

-- trueToTrue and trueToTrue' are equivalent, but not internal, it's external.
trueToTrue : ⊤ → ⊤
trueToTrue tt = tt
trueToTrue' : ⊤ → ⊤
trueToTrue' x = x

isEven : ℕ → Type
isEven zero = ⊤
isEven (suc zero) = ⊥
isEven (suc (suc n)) = isEven n

_×_ : Type → Type → Type
A × C = Σ A (λ a → C)

-- use: div2 (10 , tt)
div2 : Σ ℕ isEven → ℕ
div2 (zero , snd) = 0
div2 ((suc (suc fst)) , snd) = (div2 (fst , snd)) + 1
