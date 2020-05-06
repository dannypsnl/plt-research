module list-thms where

open import Data.List
open import Data.Nat
open import Relation.Binary.PropositionalEquality

length-++ : ∀ {A : Set} (l₁ l₂ : List A)
  → length (l₁ ++ l₂) ≡ (length l₁) + (length l₂)
length-++ [] l₂ = refl
length-++ (h ∷ t) l₂ rewrite length-++ t l₂ = refl
