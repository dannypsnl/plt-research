module bool-test where

open import Data.Bool
open import Relation.Binary.PropositionalEquality

double-negation-true : (not (not true)) ≡ true
double-negation-true = refl

double-negation-false : (not (not false)) ≡ false
double-negation-false = refl

double-negation : ∀ (b : Bool) → (not (not b)) ≡ b
double-negation true = refl
double-negation false = refl
