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

+suc : ∀ (x y : ℕ) → x + (suc y) ≡ suc (x + y)
+suc zero y = refl
+suc (suc x) y rewrite +suc x y = refl
-- commutativity
+commutativity : ∀ (x y : ℕ) → x + y ≡ y + x
+commutativity zero y rewrite +0 y = refl
+commutativity (suc x) y rewrite +suc y x | +commutativity x y = refl
