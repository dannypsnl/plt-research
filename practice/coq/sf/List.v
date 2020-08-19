Module NatList.

Inductive natprod : Type :=
| pair (n1 n2 : nat).

Notation "( x , y )" := (pair x y).

Definition fst (p : natprod) : nat :=
  match p with
  | (x, y) => x
  end.
Definition snd (p : natprod) : nat :=
  match p with
  | (x, y) => y
  end.
Definition swap_pair (p : natprod) : natprod :=
  match p with
  | (x, y) => (y, x)
  end.

Theorem surjective_pairing : forall (p : natprod),
  p = (fst p, snd p).
Proof.
  intros p. destruct p as [n m]. simpl. reflexivity.
Qed.

Theorem fst_swap_is_snd : forall (p : natprod),
  (snd p, fst p) = swap_pair p.
Proof.
  intros p.
  destruct p as [f s].
  simpl.
  reflexivity.
Qed.

Inductive natlist : Type :=
  | nil
  | cons (n : nat) (l : natlist).

Notation "x :: l" := (cons x l)
  (at level 60, right associativity).

End NatList.
