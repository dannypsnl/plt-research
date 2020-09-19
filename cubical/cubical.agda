{-# OPTIONS --cubical --omega-in-omega #-}
module cubical where
open import Cubical.Core.Everything
import Cubical.Foundations.Prelude as Prelude

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

_ : {A : Set} → {a : A} → a ≡ a
_ = Prelude.refl

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
