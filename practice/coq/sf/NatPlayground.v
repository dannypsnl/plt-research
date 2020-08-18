Module NatPlayground.

Fixpoint evenb (n:nat) : bool :=
  match n with
  | 0 => true
  | S 0 => false
  | S (S n') => evenb n'
  end.

Definition oddb (n:nat) : bool := negb (evenb n).

Example test_oddb1 : oddb 1 = true.
Proof. simpl. reflexivity. Qed.

Fixpoint minus (n m:nat) : nat :=
  match n, m with
  | 0 , _ => 0
  | S _ , 0 => n
  | S n' , S m' => minus n' m'
  end.

Fixpoint exp (base power:nat) : nat :=
  match power with
  | 0 => 1
  | S p => base * (exp base p)
  end.

Fixpoint factorial (n:nat) : nat :=
  match n with
  | 0 => 1
  | 1 => 1
  | S n' => n * n'
  end.

Example test_factorial : (factorial 3) = 6.
Proof. simpl. reflexivity. Qed.

End NatPlayground.
