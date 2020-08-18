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
  match n , m with
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

Module SimpleProof.

Theorem plus_0_n : forall n : nat, 0 + n = n.
Proof.
  intros n. simpl. reflexivity. Qed.
Theorem mult_0_n : forall n : nat, 0 * n = 0.
Proof.
  intros n. reflexivity. Qed.

Theorem plus_id : forall n m : nat,
  m = n ->
  n + n = m + m.
Proof.
  (* intros n m in forall *)
  intros n m.
  (* intros premise m = n as H *)
  intros H.
  (* rewrite by H, m + m would be rewritten as n + n *)
  rewrite -> H.
  reflexivity.
Qed.

Theorem plus_id_exercise : forall n m o : nat,
  n = m -> m = o -> n + m = m + o.
Proof.
  intros n m o.
  (* H1 : n = m *)
  intros H1.
  rewrite -> H1.
  (* H2 : m = o *)
  intros H2.
  rewrite -> H2.
  reflexivity.
Qed.

Fixpoint eqb (n m : nat) : bool :=
  match n , m with
  | 0 , 0 => true
  | S _ , 0 => false
  | 0 , S _ => false
  | S n' , S m' => eqb n' m'
  end.

Notation "x =? y" := (eqb x y) (at level 70) : nat_scope.

Theorem plus_1_neq_0 : forall n : nat,
  (n + 1) =? 0 = false.
Proof.
  intros n.
  (* [ a | b ] is a list of patterns, here 0 has no argument, use [| n']
      which means [0 = n | S n' = n]
          then two - matchs to each branch
          at here, reflexivity is enough to figure out `eqb`

     eqn named the equation to E *)
  destruct n as [| n'] eqn: E.
  - reflexivity.
  - reflexivity.
Qed.

Theorem improve_plus_1_neq_0 : forall n : nat,
  (n + 1) =? 0 = false.
Proof.
  intros [| n'].
  - reflexivity.
  - reflexivity.
Qed.

Theorem negb_involutive : forall b : bool,
  negb (negb b) = b.
Proof.
  intros b. destruct b eqn : B.
  - reflexivity.
  - reflexivity.
Qed.

Theorem andb_commutative : forall b c, andb b c = andb c b.
Proof.
  intros b c. destruct b eqn: Eb.
  (* b = true *)
  - destruct c eqn: Ec.
    (* c = true *)
    -- reflexivity.
    (* c = false *)
    -- reflexivity.
  (* b = false *)
  - destruct c eqn: Ec.
    (* c = true *)
    -- reflexivity.
    (* c = false *)
    -- reflexivity.
Qed.

Theorem super_andb_commutative : forall b c, andb b c = andb c b.
Proof.
  intros [] [].
  - reflexivity.
  - reflexivity.
  - reflexivity.
  - reflexivity.
Qed.

End SimpleProof.
