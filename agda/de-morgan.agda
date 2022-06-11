open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Data.Empty

¬_ : Set → Set
¬ A = A → ⊥

de-Morgan : {A B : Set} → (¬ A ⊎ B) → (A → B)
de-Morgan (inj₁ f) = λ a → ⊥-elim (f a)
de-Morgan (inj₂ b) = λ a → b

postulate
  de-Morgan-reverse : {A B : Set} → (A → B) → (¬ A ⊎ B)

exclude-of-middle : {A : Set} → (A → A) → (¬ A ⊎ A)
exclude-of-middle f = de-Morgan-reverse f
