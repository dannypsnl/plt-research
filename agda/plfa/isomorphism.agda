module isomorphism where

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

infix 0 _≃_
record _≃_ (A B : Set) : Set where
  field
    to : A → B
    from : B → A
    from∘to : ∀ (x : A) → from (to x) ≡ x
    to∘from : ∀ (y : B) → to (from y) ≡ y
open _≃_

data _≃′_ (A B : Set) : Set where
  mk-≃′ : ∀ (to : A → B) →
          ∀ (from : B → A) →
          ∀ (from∘to : ∀ (x : A) → from (to x) ≡ x) →
          ∀ (to∘from : ∀ (y : B) → to (from y) ≡ y) →
          A ≃′ B

to′ : ∀ {A B : Set} → (A ≃′ B) → (A → B)
to′ (mk-≃′ f g g∘f f∘g) = f

from′ : ∀ {A B : Set} → (A ≃′ B) → (B → A)
from′ (mk-≃′ f g g∘f f∘g) = g

from∘to′ : ∀ {A B : Set} → (A≃B : A ≃′ B) → (∀ (x : A) → from′ A≃B (to′ A≃B x) ≡ x)
from∘to′ (mk-≃′ f g g∘f f∘g) = g∘f

to∘from′ : ∀ {A B : Set} → (A≃B : A ≃′ B) → (∀ (y : B) → to′ A≃B (from′ A≃B y) ≡ y)
to∘from′ (mk-≃′ f g g∘f f∘g) = f∘g

≃-refl : ∀ {A : Set} → A ≃ A
≃-refl =
  record
    { to = λ {x → x}
    ; from = λ {y → y}
    ; from∘to = λ {x → refl}
    ; to∘from = λ {y → refl}
    }

≃-sym : ∀ {A B : Set}
  → A ≃ B
  -------
  → B ≃ A
≃-sym A≃B =
  record
    { to = from A≃B
    ; from = to A≃B
    ; from∘to = to∘from A≃B
    ; to∘from = from∘to A≃B
    }
