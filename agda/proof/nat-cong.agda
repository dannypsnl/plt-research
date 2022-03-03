module nat-cong where
open import Relation.Binary.PropositionalEquality
open import Data.Nat

double : ℕ → ℕ
double 0 = 0
double (suc n) = suc (suc (double n))

half : ℕ → ℕ
half 0 = 0
half (suc 0) = 0
half (suc (suc n)) = (suc (half n))

halfDouble : {n : ℕ} → half (double n) ≡ n
halfDouble {0} = refl
halfDouble {suc n} = cong suc halfDouble
