module equality where

open import Agda.Builtin.Nat using (Nat; _+_; suc)

data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x

infix 4 _≡_

sym : ∀ {A : Set} {x y : A}
  → x ≡ y
  -------
  → y ≡ x
sym refl = refl

trans : ∀ {A : Set} {x y z : A}
  → x ≡ y
  → y ≡ z
  -------
  → x ≡ z
trans refl refl = refl

cong : ∀ {A B : Set} (f : A → B) {x y : A}
  → x ≡ y
  -------
  → f x ≡ f y
cong f refl = refl

cong₂ : ∀ {A B C : Set} (f : A → B → C) {u x : A} {v y : B}
  → u ≡ x
  → v ≡ y
  -------
  → f u v ≡ f x y
cong₂ f refl refl = refl

cong-app : ∀ {A B : Set} {f g : A → B}
  → f ≡ g
  -------
  → ∀ (x : A) → f x ≡ g x
cong-app refl x = refl

module ≡-Reasoning {A : Set} where

  infix 1 begin_
  infixr 2 _≡⟨⟩_ _≡⟨_⟩_
  infix 3 _∎

  begin_ : ∀ {x y : A}
    → x ≡ y
    -------
    → x ≡ y
  begin x≡y = x≡y

  _≡⟨⟩_ : ∀ (x : A) {y : A}
      → x ≡ y
      -------
      → x ≡ y
  x ≡⟨⟩ x≡y = x≡y

  _≡⟨_⟩_ : ∀ (x : A) {y z : A}
    → x ≡ y
    → y ≡ z
    -------
    → x ≡ z
  x ≡⟨ x≡y ⟩ y≡z = trans x≡y y≡z

  _∎ : ∀ (x : A)
    -------
    → x ≡ x
  x ∎ = refl

open ≡-Reasoning

postulate
  +-identity : ∀ (m : Nat) → m + 0 ≡ m
  +-suc : ∀ (m n : Nat) → m + suc n ≡ suc (m + n)

+-comm : ∀ (m n : Nat) → m + n ≡ n + m
+-comm m 0 =
  begin
    m + 0
  ≡⟨ +-identity m ⟩
    m
  ≡⟨⟩
    0 + m
  ∎
+-comm m (suc n) =
  begin
    m + (suc n)
  ≡⟨ +-suc m n ⟩
    suc (m + n)
  ≡⟨ cong suc (+-comm m n) ⟩
    suc (n + m)
  ≡⟨⟩
    suc n + m
  ∎

data even : Nat → Set
data odd : Nat → Set

data even where
  even-zero : even 0
  even-suc : ∀ {n : Nat} → odd n → even (suc n)
data odd where
  odd-suc : ∀ {n : Nat} → even n → odd (suc n)

{-# BUILTIN EQUALITY _≡_ #-}

even-comm : ∀ (m n : Nat)
  → even (m + n)
  → even (n + m)
even-comm m n ev rewrite +-comm n m = ev
