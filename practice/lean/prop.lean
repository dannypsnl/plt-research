variables p q r : Prop
#check p ∧ q
#check (p ∧ q) ∨ r
#check (p ∧ q) → (q ∧ p)

theorem t1 : p → q → p :=
  assume hp : p,
  assume hq : q,
  hp

#check ¬p → p ↔ false
#check p → q ↔ p ∧ q ↔ q ∧ p

example (h : p ∧ q) : p := and.elim_left h
example (h : p ∧ q) : p := h.left
example (h : p ∧ q) : q := and.elim_right h
example (h : p ∧ q) : q := h.right

variable l : list nat
#check list.head l
#check l.head

example (h : p ∧ q) : q ∧ p :=
show q ∧ p, from and.intro h.right h.left

example (h : p ∧ q) : q ∧ p :=
suffices hq : q, from and.intro hq h.left,
show q, from h.right

section
  open classical
  example (h : ¬¬p) : p :=
  by_contradiction
    (assume h1 : ¬p,
      show false, from h h1)
end
