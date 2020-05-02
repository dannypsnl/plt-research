module bool-test where

open import Data.Bool
open import Relation.Binary.PropositionalEquality

double-negation : (not (not true)) ≡ true
double-negation = refl

double-negation-false : (not (not false)) ≡ false
double-negation-false = refl
