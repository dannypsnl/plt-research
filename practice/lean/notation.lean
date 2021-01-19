notation `[` a `**` b `]` := a * b + 1

def mul_square (a b : nat) : nat :=
  a * a * b * b
infix `<*>`:50 := mul_square

#reduce [2 ** 3]
#reduce 2 <*> 3

#print notation
#print notation + * -
#print Σ