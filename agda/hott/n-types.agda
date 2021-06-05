module n-types where

open import Agda.Primitive public using (lzero)
  renaming (Level to ULevel; lsuc to lsucc; _⊔_ to lmax)
open import Agda.Builtin.Equality
open import Data.Bool

Type : (i : ULevel) → Set (lsucc i)
Type i = Set i

Type₀ = Type lzero

data ⊥ : Type₀ where
data ⊤ : Type₀ where
  tt : ⊤

¬_ : Set → Set
¬ A = A → ⊥

isProp : ∀ {i} → Type i → Type i
isProp A = (x y : A) → x ≡ y

⊥-isProp : isProp ⊥
⊥-isProp ()

⊤-isProp : isProp ⊤
⊤-isProp tt tt = refl

Bool-isn'tProp : ¬ (isProp Bool)
Bool-isn'tProp P with P true false
Bool-isn'tProp P | ()
