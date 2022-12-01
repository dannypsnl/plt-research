open Lexing
module L = Usualtt.Lexer
module P = Usualtt.Parser
open Usualtt.Core
open Usualtt.Term

let rec parse' f source =
  let lexbuf = Lexing.from_channel source in
  try f L.token lexbuf
  with P.Error ->
    raise (Failure ("Parse error at " ^ pos_string lexbuf.lex_curr_p))

    and pos_string pos =
    let l = string_of_int pos.pos_lnum and c = string_of_int (column pos + 1) in
    "line " ^ l ^ ", column " ^ c
  
and column pos = pos.pos_cnum - pos.pos_bol - 1
  
let parse_program source = parse' P.program source

let () =
  Printexc.register_printer (function
  | NoVar x -> Some (Printf.sprintf "no variable found: `%s`" x)
  | TypeMismatch (t1, t2) ->
    Some (Printf.sprintf "type mismatched: `%s` not equals to `%s`" (string_of_ty t1) (string_of_ty t2))
  | InferLambda tm ->
    Some (Printf.sprintf "tries to infer lambda: `%s`" (string_of_term tm))
  | BadApp ty ->
    Some (Printf.sprintf "try apply: `%s`" (string_of_ty ty))
  | BadLift -> Some "internal bug, it shouldn't have any way to create type schema from code to let you lifting it"
  | _ -> None);

  print_string "usualtt";
  print_newline ();
  let file_name = Array.get Sys.argv 1 in
  (* read file *)
  let in_chan = open_in file_name in
  (* parsing *)
  let term = parse_program in_chan in
  (* check type *)
  let inferred_ty = quote [] (infer [] [] term)
  in print_string "term:\n\n";
     print_term term;
     print_string "\n\nhas type:\n\n";
     print_ty inferred_ty;
     print_newline ()
  