type typ = Nat [@printer fun fmt _ -> fprintf fmt "Nat"] [@@deriving show]

type 'a spine =
  { constructor : string
  ; spine : 'a list
  }

type pattern =
  | Var of string * typ
  [@printer fun fmt (n, ty) -> fprintf fmt "(%s : %s)" n (show_typ ty)]
  | Spine of pattern spine
  [@printer
    fun fmt sp ->
      fprintf
        fmt
        "%s %s"
        sp.constructor
        (String.concat " " (List.map show_pattern sp.spine))]
[@@deriving show]

exception CannotSplit
exception CanSplit
exception FailMatch
exception NotCovered of pattern

let type_meta : (typ, pattern list) Hashtbl.t = Hashtbl.create 100

let split (case : pattern) : pattern list =
  match case with
  | Var (x, ty) ->
    Printf.printf "split %s\n" x;
    Hashtbl.find type_meta ty
  | _ -> raise CannotSplit
;;

(* A case is said to be cover, if any pattern can cover it *)
let rec check_coverage (case : pattern) (patterns : pattern list) : unit =
  match patterns with
  | pat :: rest ->
    (try cover case pat with
     | FailMatch -> check_coverage case rest
     | CanSplit ->
       let cases = split case in
       List.iter (fun c -> check_coverage c patterns) cases)
  | [] -> raise @@ NotCovered case

and cover (case : pattern) (pattern : pattern) : unit =
  match case, pattern with
  | _, Var _ -> ()
  | Var _, Spine _ -> raise CanSplit
  | Spine { constructor = c1; spine = s1 }, Spine { constructor = c2; spine = s2 } ->
    if String.equal c1 c2 then List.iter2 cover s1 s2 else raise FailMatch
;;

let%expect_test "" =
  Hashtbl.add
    type_meta
    Nat
    [ Spine { constructor = "zero"; spine = [] }
    ; Spine { constructor = "suc"; spine = [ Var ("n", Nat) ] }
    ];
  (try
     check_coverage
       (Var ("m", Nat))
       [ Spine { constructor = "zero"; spine = [] }
       ; Spine { constructor = "suc"; spine = [ Var ("m1", Nat) ] }
       ]
   with
   | NotCovered case -> Printf.printf "`%s` is not covered\n" ([%show: pattern] case));
  [%expect {| split m |}]
;;
