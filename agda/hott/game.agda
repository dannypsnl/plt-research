module game where

open import Data.Nat using (ℕ; suc; zero)
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

_+_ : ℕ → ℕ → ℕ
zero + n = n
suc n + m = suc (n + m)

-- use: div2 (10 , tt)
div2 : Σ ℕ isEven → ℕ
div2 (zero , snd) = 0
div2 ((suc (suc fst)) , snd) = (div2 (fst , snd)) + 1

private
  postulate
    A B C : Type

uncurry : (A → B → C) → (A × B → C)
uncurry f (a , b) = f a b
curry : (A × B → C) → (A → B → C)
curry f a b = f (a , b)

data Id {A : Type} : (x y : A) → Type where
  rfl : {x : A} → Id x x

idSym : (A : Type) (x y : A) → Id x y → Id y x
idSym A x y rfl = rfl

idTrans : (A : Type) (x y z : A) → Id x y → Id y z → Id x z
idTrans A x y z rfl rfl = rfl
