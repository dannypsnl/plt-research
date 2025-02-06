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
      if List.is_empty sp.spine
      then fprintf fmt "%s" sp.constructor
      else
        fprintf
          fmt
          "%s %s"
          sp.constructor
          (String.concat " " (List.map show_pattern sp.spine))]
[@@deriving show]

exception Split of pattern list
exception FailMatch
exception NotCovered of pattern

let type_meta : (typ, pattern list) Hashtbl.t = Hashtbl.create 100

(* A case is said to be covered, if any pattern can cover it *)
let rec check_coverage (case : pattern) (patterns : pattern list) : unit =
  match patterns with
  | pat :: rest ->
    (try cover case pat with
     | FailMatch -> check_coverage case rest
     | Split cases -> List.iter (fun c -> check_coverage c patterns) cases)
  | [] -> raise @@ NotCovered case

and cover (case : pattern) (pattern : pattern) : unit =
  match case, pattern with
  | _, Var _ -> ()
  | Var (x, ty), Spine _ ->
    Printf.printf "split %s\n" x;
    let cases = Hashtbl.find type_meta ty in
    raise @@ Split cases
  | Spine { constructor = c1; spine = cs }, Spine { constructor = c2; spine = pats } ->
    if String.equal c1 c2
    then
      (* This is super ugly, the idea is if sub-split in a Spine happened,
         we should wrap the sub-split with proper Spine, to ensure nested pattern work as well.

         Since OCaml List API is like this, the way is using iteri to compute which nth sub-case raise Split,
         then replace nth Var with splits
      *)
      List.iteri
        (fun n case ->
           let pat = List.nth pats n in
           try cover case pat with
           | Split cases ->
             let wrapper =
               fun c ->
               let cs = List.mapi (fun k case -> if n == k then c else case) cs in
               Spine { constructor = c1; spine = cs }
             in
             let cases = List.map wrapper cases in
             raise @@ Split cases)
        cs
    else raise FailMatch
;;

let%expect_test "basic split" =
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
       ; Spine { constructor = "suc"; spine = [ Var ("m", Nat) ] }
       ]
   with
   | NotCovered case -> Printf.printf "`%s` is not covered\n" ([%show: pattern] case));
  [%expect {| split m |}]
;;

let%expect_test "nested pattern" =
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
       ; Spine
           { constructor = "suc"; spine = [ Spine { constructor = "zero"; spine = [] } ] }
       ; Spine
           { constructor = "suc"
           ; spine = [ Spine { constructor = "suc"; spine = [ Var ("m1", Nat) ] } ]
           }
       ]
   with
   | NotCovered case -> Printf.printf "`%s` is not covered\n" ([%show: pattern] case));
  [%expect
    {|
    split m
    split n
    |}]
;;
