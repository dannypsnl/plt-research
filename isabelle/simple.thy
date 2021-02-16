theory simple

imports Main

begin

datatype bool = True | False

fun conj :: "bool \<Rightarrow> bool \<Rightarrow> bool" where
  "conj True True = True" |
  "conj _ _ = False"

datatype nat = z | Suc nat

fun add :: "nat \<Rightarrow> nat \<Rightarrow> nat" where
  "add z n = n" |
  "add (Suc m) n = Suc (add m n)"

lemma add_m_0 : "add m z = m"
  (* proof by induction m
     which means z would work
     and Suc m would work
     then for all nat would work
  *)
  apply(induction m)
  apply(auto)
  done

thm add_m_0

datatype 'a list = Nil | Cons 'a "'a list"

fun append :: "'a list \<Rightarrow> 'a list \<Rightarrow> 'a list" where
  "append Nil ys = ys" |
  "append (Cons x xs) ys = Cons x (append xs ys)"

fun reverse :: "'a list \<Rightarrow> 'a list" where
  "reverse Nil = Nil" |
  "reverse (Cons x xs) = append (reverse xs) (Cons x Nil)"

value "reverse (Cons (Suc z) (Cons z Nil))"

lemma append_nil [simp]: "append xs Nil = xs"
  apply(induction xs) 
  apply(auto)
  done

lemma append_assoc [simp]: "append (append xs ys) zs = append xs (append ys zs)"
  apply(induction xs)
  apply(auto)
  done

lemma reverse_append [simp]: "reverse (append xs ys) = append (reverse ys) (reverse xs)"
  apply(induction xs)
  apply(auto)
  done

theorem reverse_reverse [simp]: "reverse (reverse xs) = xs"
  apply(induction xs)
  apply(auto)
  done

end