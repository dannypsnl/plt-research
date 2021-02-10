module lambda where

open import Data.Bool using (T; not)
open import Data.Empty using (⊥; ⊥-elim)
open import Data.List using (List; _∷_; [])
open import Data.Product using (∃-syntax; _×_)
open import Data.String using (String; _≟_)
open import Relation.Nullary using (Dec; yes; no; ¬_)
open import Relation.Nullary.Decidable using (⌊_⌋; False; toWitnessFalse)
open import Relation.Nullary.Negation using (¬?)
open import Relation.Binary.PropositionalEquality using (_≡_; _≢_; refl)

Id : Set
Id = String

{-
ƛ: \Gl-
⇒: \=>
μ: \mu
∙: \.
-}
infix 5 ƛ_⇒_
infix 5 μ_⇒_
infixl 7 _∙_
infix 8 `suc_
infix 9 `_

data Term : Set where
  `_ : Id → Term
  ƛ_⇒_ : Id → Term → Term
  _∙_ : Term → Term → Term
  `zero : Term
  `suc_ : Term -> Term
  case_[zero⇒_|suc_⇒_] : Term → Term → Id → Term → Term
  μ_⇒_ : Id → Term → Term

data Value : Term → Set where
  V-ƛ : ∀ {x N} → Value (ƛ x ⇒ N)

two : Term
two = `suc `suc `zero

_ : Term
_ = two

plus : Term
plus = μ "+" ⇒ ƛ "m" ⇒ ƛ "n" ⇒
         case `"m"
           [zero⇒ ` "n"
           |suc "m" ⇒ `suc (`"+" ∙ `"m" ∙ `"n")
           ]

_ : Term
_ = plus ∙ two ∙ two

mul : Term
mul = μ "*" ⇒ ƛ "m" ⇒ ƛ "n" ⇒
        case ` "m"
          [zero⇒ `zero
          |suc "m" ⇒ `"+" ∙ `"n" ∙ (`"+" ∙ `"m" ∙ `"n")
          ]

_ : Term
_ = mul ∙ two ∙ two
