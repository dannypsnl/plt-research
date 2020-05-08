module list-thms where

open import Data.List
open import Data.Nat
open import Relation.Binary.PropositionalEquality

length-++ : ∀ {A : Set} (l₁ l₂ : List A)
  → length (l₁ ++ l₂) ≡ (length l₁) + (length l₂)
length-++ [] l₂ = refl
length-++ (h ∷ t) l₂ rewrite length-++ t l₂ = refl

++-assoc : ∀ {A : Set} (l₁ : List A) (l₂ : List A) (l₃ : List A)
           → (l₁ ++ l₂) ++ l₃ ≡ l₁ ++ (l₂ ++ l₃)
++-assoc [] l₂ l₃ = refl
++-assoc (h ∷ t) l₂ l₃ rewrite ++-assoc t l₂ l₃ = refl
