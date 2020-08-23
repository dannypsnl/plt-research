Theorem plus_n_0 : forall n : nat, n = n + 0.
Proof.
  induction n as [| n' IHn'].
  - (* n = 0 *)
    reflexivity.
  - (* n = S n' *)
    simpl. rewrite <- IHn'. reflexivity.
Qed.

Theorem minus_n_n : forall n : nat, n - n = 0.
Proof.
  induction n as [| n' IHn'].
  - (* n = 0 *)
    simpl. reflexivity.
  - (* n = S n' *)
    simpl. rewrite -> IHn'. reflexivity.
Qed.

Theorem mult_n_1 : forall n : nat, n * 1 = n.
Proof.
  induction n as [| n' IHn'].
  - reflexivity.
  - simpl. rewrite -> IHn'. reflexivity.
Qed.

Theorem mult_0_plus' : forall n m : nat,
  (0 + n) * m = n * m.
Proof.
  intros n m.
  (* or
  assert (0 + n = n) as H.
  *)
  assert (H : 0 + n = n). { reflexivity. }
  rewrite -> H.
  reflexivity.
Qed.

Lemma plus_comm : forall n m : nat,
  (n + m) = (m + n).
Proof. Admitted.
Theorem plus_rearrange : forall n m p q : nat,
  (n + m) + (p + q) = (m + n) + (p + q).
Proof.
  intros n m p q.
  assert (n + m = m + n) as H. { rewrite -> plus_comm. reflexivity. }
  rewrite -> H.
  reflexivity.
Qed.
