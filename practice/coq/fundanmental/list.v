Inductive list (A:Set) : Set :=
| nil : list A
| cons : A -> list A -> list A.

Check nil.
Check cons.
