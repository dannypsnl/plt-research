{-# OPTIONS --cubical --omega-in-omega --allow-unsolved-metas #-}
module cubical where
open import Cubical.Core.Everything
open import Cubical.Foundations.Prelude

interval = I

_ : I
_ = i0
_ : I
_ = i1

min : I → I → I
min = _∧_
max : I → I → I
max = _∨_
neg : I → I
neg = ~_

myImagination : (A : Set) → (a b : A) → Set
myImagination = Path

constantPathLam : (A : Set) → (a : A) → Path A a a
constantPathLam A a p = a

invert : (A : Set) (a b : A) (p : a ≡ b) → b ≡ a
invert A a b p = λ i → p (~ i)

congruence : {A B : Set}
             → (f : A → B)
             → {x y : A}
             → (p : x ≡ y)
             → f x ≡ f y
congruence f p = λ i → f (p i)

functionExtensionality : {A B : Set}
                         → {f g : A → B}
                         → (p : ∀ a → f a ≡ g a)
                         → f ≡ g
functionExtensionality p = λ i a → p a i

variable A : Set

{- Path `a ≡ a` takes `i : I` -}
reflEx : (a : A) → a ≡ a
reflEx a = λ i → a

reflReflEx : (a : A) → reflEx a ≡ reflEx a
reflReflEx a = λ j i → a

module UseOfSquares
  (a b c d : A)
  (p : a ≡ b)
  (q : a ≡ b)
  (s : p ≡ q)
  where

  left : a ≡ b
  left = s i0

  right : a ≡ b
  right = s i1

  top : a ≡ a
  top = λ i → s i i0

  bottom : b ≡ b
  bottom = λ i → s i i1

  rotate : (sym q) ≡ (sym p)
  rotate = λ i j → s (~ i) (~ j)

  digonal : a ≡ b
  digonal = λ i → s i i

module ConstructionOfSquares
  (a b : A)
  (p : a ≡ b)
  where

  easiest : p ≡ p
  easiest = reflEx _
