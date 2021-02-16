fiveIsFive : 5 = 5
fiveIsFive = Refl

twoPlusTwoIsFour : 2 + 2 = 4
twoPlusTwoIsFour = Refl

plusReduces : (n : Nat) -> Z + n = n
plusReduces n = Refl

plusRightId : (n : Nat) -> n + 0 = n
plusRightId Z = Refl
plusRightId (S n) = cong S (plusRightId n)

plusReducesS : (n : Nat) -> (m : Nat) -> n + (S m) = S (n + m)
plusReducesS Z m = cong S Refl
-- cong: congruence, take `f` to show `a = b` iff `(f a) = (f b)`
plusReducesS (S k) m = cong S (plusReducesS k m)

concatLeftId : (lst : List a) -> [] ++ lst = lst
concatLeftId lst = Refl
concatRightId : (lst : List a) -> lst ++ [] = lst
concatRightId [] = Refl
concatRightId (x :: xs) = cong (x ::) (concatRightId xs)
