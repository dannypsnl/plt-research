constant m : nat
constant n : nat
#check m
-- a "functional"
constant F : nat →  nat →  nat
#check F m

#check array.ext'
#check nat
#check bool →  bool
#check Prop
#check λ x : nat, x + 5

#reduce (λ x : nat, x) m
#reduce let y := 2 + 2 in y * y

namespace hidden
  universe u

  constant list : Type u →  Type u

  constant cons : Π a : Type u, a →  list a →  list a
  constant nil : Π a : Type u, list a

end hidden

open list

#check list     -- Type u_1 → Type u_1

#check @cons
#check @nil
#check @head
#check @tail
#check @append
