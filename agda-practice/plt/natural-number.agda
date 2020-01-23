module natural-number where

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; _∎)

data ℕ : Set where
  -- basic case: `zero` is a natural number
  zero : ℕ
  -- inductive case: `succ m` is a natural number if `m` is a natural number
  succ : ℕ -> ℕ

_+_ : ℕ -> ℕ -> ℕ
zero + n = n
(succ m) + n = succ (m + n)

_ : succ (succ (zero)) + succ (succ (succ (zero))) ≡ succ (succ (succ (succ (succ (zero)))))
-- we can use `refl`
_ = refl
-- or manually do this
_ =
  begin
    succ (succ (zero)) + succ (succ (succ (zero)))
  ≡⟨⟩    -- is shorthand for
    (succ (succ zero)) + (succ (succ (succ zero)))
  ≡⟨⟩    -- inductive case
    succ ((succ zero) + (succ (succ (succ zero))))
  ≡⟨⟩    -- inductive case
    succ (succ (zero + (succ (succ (succ zero)))))
  ≡⟨⟩    -- base case
    succ (succ (succ (succ (succ zero))))
  ≡⟨⟩    -- is longhand for
    succ (succ (succ (succ (succ (zero)))))
  ∎

_*_ : ℕ → ℕ → ℕ
zero * n = zero
(succ m) * n = n + (m * n)

_ : succ (succ (zero)) * succ (succ (succ (zero))) ≡ succ (succ (succ (succ (succ (succ (zero))))))
_ = refl

_∸_ : ℕ → ℕ → ℕ
m ∸ zero = m
zero ∸ succ n = zero
succ m ∸ succ n = m ∸ n

_ : succ (succ (succ (zero))) ∸ succ (succ (zero)) ≡ succ zero
_ = refl

infixl 6  _+_  _∸_
infixl 7  _*_

