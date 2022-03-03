module uep where
open import Relation.Binary.PropositionalEquality

uep : {A : Set} → {x y : A}
      → (p q : x ≡ y)
      → p ≡ q
uep refl refl = refl
