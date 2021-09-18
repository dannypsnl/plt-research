open import Data.Bool hiding (T)
open import Data.Nat 
open import Data.List

variable
  T : Set
  S S' S'' : List Set
-- Source Language
data Exp : Set → Set where
  val : T → Exp T
  add : Exp ℕ → Exp ℕ → Exp ℕ
  if : Exp Bool → Exp T → Exp T → Exp T

eval : Exp T → T
eval (val x)    = x
eval (add x y)  = eval x + eval y
eval (if b x y) = if eval b then eval x else eval y

-- Target Language
data Stack : List Set → Set where
  ε : Stack []
  _▷_ : T → Stack S → Stack (T ∷ S)

infixr 20 _▷_

data Code : List Set → List Set → Set₁ where
  PUSH  : T → Code S (T ∷ S)
  ADD   : Code (ℕ ∷ ℕ ∷ S) (ℕ ∷ S)
  _+++_ : Code S S' → Code S' S'' → Code S S''
  IF    : Code S S' → Code S S' → Code (Bool ∷ S) S'

exec : Code S S' → Stack S → Stack S'
exec (PUSH x) s         = x ▷ s
exec (IF c1 c2) (b ▷ s) = if b then exec c1 s else exec c2 s
exec (c1 +++ c2) s      = exec c2 (exec c1 s)
exec ADD (m ▷ n ▷ s)    = (n + m) ▷ s

-- Compiler
compile : Exp T → Code S (T ∷ S)
compile (val x)    = PUSH x
compile (if b x y) = compile b +++ IF (compile x) (compile y)
compile (add x y)  = (compile x +++ compile y) +++ ADD
