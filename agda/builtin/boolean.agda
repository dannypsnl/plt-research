module boolean where

open import IO
open import Data.String

data MyBool : Set where
  myfalse mytrue : MyBool

showMyBool : (b : MyBool) → String
showMyBool myfalse = "false"
showMyBool mytrue = "true"

main = run (putStrLn (showMyBool myfalse))
