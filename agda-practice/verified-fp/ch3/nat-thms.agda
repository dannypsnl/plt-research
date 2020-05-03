module nat-thms where

open import Data.Nat
open import Relation.Binary.PropositionalEquality

0+ : ∀ (x : ℕ) → 0 + x ≡ x
0+ x = refl

+0 : ∀ (x : ℕ) → x + 0 ≡ x
+0 zero = refl
-- prove `x + 0 ≡ x` means prove `suc (x-1 + 0) ≡ suc x-1`
+0 (suc x) rewrite +0 x = refl

-- associativity
+associativity : ∀ (x y z : ℕ) → x + (y + z) ≡ (x + y) + z
-- zero + (y + z) ≡ (zero + y) + z
-- if replace `refl` with hole, use `C-c C-,` can see Goal is `y + z ≡ y + z`
+associativity zero y z = refl
-- reduce x to zero
+associativity (suc x) y z rewrite +associativity x y z = refl
