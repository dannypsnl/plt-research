exception BadQuote

type var = string

type ty = 
  (* primitive *)
  | Bool
  | Int
  (* list a *)
  | List of ty
  (* a -> b *)
  | Arrow of ty * ty
  (* type variable *)
  | TVar of var
  (* type schema *)
  | TSchema of var * ty

type ty_val =
  | TVar of var
  | TLam of var * (ty_val -> ty_val)
  | TArrow of ty_val * ty_val
  | TApp of ty_val * ty_val

type term =
  (* constant *)
  | Int of int
  | Bool of bool
  (* x *)
  | Var of var
  (* t u *)
  | App of term * term
  (* λ x . t *)
  | Lam of var * term
  (* let x : a = t; u *)
  | Let of var * ty * term * term

let rec string_of_term : term -> string =
    fun tm ->
      match tm with
      | Int i -> string_of_int i
      | Bool b -> string_of_bool b
      | Var x -> x
      | App (t, u) -> (string_of_term t) ^ " " ^ (string_of_term u)
      | Lam (x, t) -> "λ" ^ x ^ "." ^ (string_of_term t)
      | Let (x, a, t, u) -> "let " ^ x ^ " : " ^ (string_of_ty a) ^ " = " ^ (string_of_term t) ^ ";\n" ^ (string_of_term u)
and string_of_ty : ty -> string =
  fun ty ->
    match ty with
    | Bool -> "Bool"
    | Int -> "Int"
    | List a -> "List<" ^ string_of_ty a ^ ">"
    | Arrow (a, b) -> string_of_ty a ^ " -> " ^ string_of_ty b
    | TVar x -> "?" ^ x
    | TSchema (v, t) -> "∀" ^ v ^ "." ^ string_of_ty t
and print_term : term -> unit =
  fun tm -> print_string (string_of_term tm)
and print_ty : ty -> unit =
  fun ty -> print_string (string_of_ty ty)
