module list-thms where

open import Level using (Level)
open import Function
open import Agda.Builtin.Unit
open import Data.Empty
open import Data.List
open import Data.Nat.Properties
open import Data.Nat
open import Data.Bool using (Bool; true; false)
open import Relation.Unary
open import Relation.Nullary
open import Relation.Nullary.Negation
open import Relation.Binary.PropositionalEquality
open ≡-Reasoning

length-++ : ∀ {A : Set} (l₁ l₂ : List A)
  → length (l₁ ++ l₂) ≡ (length l₁) + (length l₂)
length-++ [] l₂ = refl
length-++ (h ∷ t) l₂ rewrite length-++ t l₂ = refl

++-assoc : ∀ {A : Set} (l₁ : List A) (l₂ : List A) (l₃ : List A)
           → (l₁ ++ l₂) ++ l₃ ≡ l₁ ++ (l₂ ++ l₃)
++-assoc [] l₂ l₃ = refl
++-assoc (h ∷ t) l₂ l₃ rewrite ++-assoc t l₂ l₃ = refl

≤-suc : ∀ (n : ℕ) → n ≤ suc n
≤-suc zero = z≤n
≤-suc (suc n) = s≤s (≤-suc n)

private
  variable
    a  p : Level
    A : Set a
module _ {P : Pred A p} (P? : Decidable P) where

  length-filter : ∀ xs → length (filter P? xs) ≤ length xs
  length-filter [] = z≤n
  length-filter (x ∷ xs) with does (P? x)
  -- when P? returns true, value would be keep
  ... | true = s≤s (length-filter xs)
  -- when P? returns false, value would be drop
  -- then we proof ≤-step: (length-filter xs) ≤ 1 + (length-filter xs)
  ... | false = ≤-step (length-filter xs)

  filter-idempotent : filter P? ∘ filter P? ≗ filter P?
  filter-idempotent [] = refl
  filter-idempotent (x ∷ xs) with does (P? x) | inspect does (P? x)
  -- in this branch `P?` removes head, so have to check rest list follow the rule
  ... | false | _ = filter-idempotent xs
  ... | true | [ eq ] rewrite eq = cong (x ∷_) (filter-idempotent xs)

  length-ʳ++ : ∀ (xs {ys} : List A) →
             length (xs ʳ++ ys) ≡ length xs + length ys
  length-ʳ++ [] = refl
  length-ʳ++ (x ∷ xs) {ys} =
    begin
      length ((x ∷ xs) ʳ++ ys)
    ≡⟨⟩
      length (xs ʳ++ x ∷ ys)
    ≡⟨ length-ʳ++ xs ⟩
      length xs + length (x ∷ ys)
    ≡⟨ +-suc _ _ ⟩
      length (x ∷ xs) + length ys
    ∎

  length-reverse : ∀ (xs : List A) → length (reverse xs) ≡ length xs
  length-reverse xs =
    begin
      length (reverse xs)
    ≡⟨⟩
      length (xs ʳ++ [])
    ≡⟨ length-ʳ++ xs ⟩
      (length xs + 0)
    ≡⟨ +-identityʳ _ ⟩
      length xs
    ∎
