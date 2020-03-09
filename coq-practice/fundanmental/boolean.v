Inductive bool : Type := true : bool | false : bool.
Inductive eq (A : Type) (x : A) : A -> Prop := eq_refl : eq A x x.
Inductive or (A : Prop) (B : Prop) : Prop :=
  | or_introl : A -> or A B
  | or_intror : B -> or A B.
Definition bool_case (b : bool) : or (eq bool b true) (eq bool b false) :=
  match b return or (eq bool b true) (eq bool b false) with
  | true => or_introl (eq bool true true) (eq bool true false) (eq_refl bool true)
  | false => or_intror (eq bool false true) (eq bool false false) (eq_refl bool false)
  end.

Check (bool_case true).
Check (bool_case false).
