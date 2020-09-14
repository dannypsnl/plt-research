module Isomorphism where

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong; cong-app)
open Eq.≡-Reasoning
open import Agda.Builtin.Nat using (Nat; suc; _+_)
open import Data.Nat.Properties using (+-comm)

_∘_ : ∀ {A B C : Set} → (A → B) → (B → C) → (A → C)
(f ∘ g) x = g (f x)

postulate
  extensionality : ∀ {A B : Set} → {f g : A → B}
    → (∀ (x : A) → f x ≡ g x)
    -------------------------
    → f ≡ g
  ∀-extensionality : ∀ {A : Set} {B : A → Set} {f g : ∀(x : A) → B x}
    → (∀ (x : A) → f x ≡ g x)
    -----------------------
    → f ≡ g

_+′_ : Nat → Nat → Nat
x +′ 0 = x
x +′ (suc y) = suc (x +′ y)

same-app : ∀ (m n : Nat) → m +′ n ≡ m + n
same-app m n rewrite +-comm m n = helper m n
  where
  helper : ∀ (m n : Nat) → m +′ n ≡ n + m
  helper m 0 = refl
  helper m (suc n) = cong suc (helper m n)
