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

data Value : Term → Set where
  V-ƛ : ∀ {x N} →
      ---------------
      Value (ƛ x ⇒ N)
  V-zero :
      -----------
      Value `zero
  V-suc : ∀ {V}
    → Value V
      --------------
    → Value (`suc V)

-- substitution
infix 9 _[_:=_]

_[_:=_] : Term → Id → Term → Term
(` x) [ y := V ] with x ≟ y
-- yes, replace x with V
... | yes _          =  V
-- no, return x itself
... | no  _          =  ` x
(ƛ x ⇒ N) [ y := V ] with x ≟ y
-- new intro x shadow the outer scope, stop substitution
... | yes _          =  ƛ x ⇒ N
-- keep substituting
... | no  _          =  ƛ x ⇒ N [ y := V ]
(L ∙ M) [ y := V ]   =  L [ y := V ] ∙ M [ y := V ]
(`zero) [ y := V ]   =  `zero
(`suc M) [ y := V ]  =  `suc M [ y := V ]
(case L [zero⇒ M |suc x ⇒ N ]) [ y := V ] with x ≟ y
... | yes _          =  case L [ y := V ] [zero⇒ M [ y := V ] |suc x ⇒ N ]
... | no  _          =  case L [ y := V ] [zero⇒ M [ y := V ] |suc x ⇒ N [ y := V ] ]
(μ x ⇒ N) [ y := V ] with x ≟ y
... | yes _          =  μ x ⇒ N
... | no  _          =  μ x ⇒ N [ y := V ]

_ : (ƛ "x" ⇒ ` "y") [ "y" := `zero ] ≡ ƛ "x" ⇒ `zero
_ = refl

_ : (ƛ "x" ⇒ ` "x") [ "x" := `zero ] ≡ ƛ "x" ⇒ ` "x"
_ = refl

_ : (ƛ "y" ⇒ ` "y") [ "x" := `zero ] ≡ ƛ "y" ⇒ ` "y"
_ = refl

_ : (ƛ "y" ⇒ ` "x" ∙ (ƛ "x" ⇒ ` "x")) [ "x" := `zero ] ≡ (ƛ "y" ⇒ `zero ∙ (ƛ "x" ⇒ ` "x"))
_ = refl
