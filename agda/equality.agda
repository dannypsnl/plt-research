module equality where
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

sym : {A : Set} → {x y : A}
      → x ≡ y
      → y ≡ x
sym refl = refl

trans : {A : Set} → {x y z : A}
        → x ≡ y
        → (q : y ≡ z)
        → x ≡ z
trans refl q = q
-- it's easy to realize another `refl` will work
trans' : {A : Set} → {x y z : A}
         → x ≡ y
         → y ≡ z
         → x ≡ z
trans' refl refl = refl

cong : {A B : Set} → (f : A → B)
       → {x y : A}
       → x ≡ y
       → f x ≡ f y
cong f refl = refl
