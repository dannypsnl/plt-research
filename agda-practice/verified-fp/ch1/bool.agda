module bool where

open import Data.Bool
open import Data.Nat

~_ : Bool → Bool
~ true = false
~ false = true

_&&_ : Bool → Bool → Bool
true && b = b
false && b = false

_||_ : Bool → Bool → Bool
true || b = true
false || b = b

myif_then_else_ : ∀ {ℓ} {A : Set ℓ} → Bool → A → A → A
myif true then t else f = t
myif false then t else f = f
