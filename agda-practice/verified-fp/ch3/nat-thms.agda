module nat-thms where

open import Data.Nat
open import Relation.Binary.PropositionalEquality

0+ : ∀ (x : ℕ) → 0 + x ≡ x
0+ x = refl

+0 : ∀ (x : ℕ) → x + 0 ≡ x
+0 zero = refl
-- prove `x + 0 ≡ x` means prove `suc (x-1 + 0) ≡ suc x-1`
+0 (suc x) rewrite +0 x = refl
