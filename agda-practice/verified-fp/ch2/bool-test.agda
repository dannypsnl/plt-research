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

_&&_ : Bool → Bool → Bool
true && b = b
false && b = false

and-same : ∀ {b} → b && b ≡ b
and-same {true} = refl
and-same {false} = refl

test-and-same : (true && true) ≡ true
test-and-same = and-same {true}

_||_ : Bool → Bool → Bool
true || b = true
false || b = b

or-same : ∀ {b} → b || b ≡ b
or-same {true} = refl
or-same {false} = refl

test-or-same : (false || false) ≡ false
test-or-same = or-same {false}

-- absurd: `()`
||≡ff₁ : ∀ {b1 b2} → b1 || b2 ≡ false → false ≡ b1
||≡ff₁ {false} p = refl
-- wrong definition: ||≡ff₁ {true} p = refl
-- would get: `false != true of type Bool`
||≡ff₁ {true} ()

-- cong
||-cong₁ : ∀ {b1 b1' b2} → b1 ≡ b1' → b1 || b2 ≡ b1' || b2
||-cong₁ refl = refl
