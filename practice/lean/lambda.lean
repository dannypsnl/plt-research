inductive Term where
  | var (name : String)
  | num (val : Nat)
  | app (fn : Term) (arg : Term)
  | abs (name : String) (type : Term) (body : Term)

def getBinderName : Term → Option String
  | Term.lambda (name := n) .. => some n
  | _ => none

def getBinderType : Term → Option Term
  | Term.lambda (type := t) .. => some t
  | _ => none
