{
  open Lexing
  open Parser

  let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let digit = ['0'-'9']
let sign = ['-' '+']
let exponent = ['e' 'E']
let alpha = ['a'-'z' 'A'-'Z']

let int_constant = sign? digit+
let float_constant = sign? digit+ '.' digit+ (exponent sign? digit+)?
let identifier = alpha (alpha | digit | '-')*
let string_literal = '"'  '"'

let whitespace = [' ' '\t']+

let digit = ['0'-'9']
let sign = ['-' '+']
let alpha = ['a'-'z' 'A'-'Z']

let int_constant = sign? digit+
let identifier = alpha (alpha | digit | '-')*

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

(* Rules *)

rule token = parse
  | "let" { LET }
  | "λ" { LAM }
  | "lam" { LAM }
  | '.' { DOT }
  | '=' { EQ }
  | ':' { COLON }
  | ';' { COMMA }
  | '(' { L_PAREN }
  | ')' { R_PAREN }
  (* type. *)
  | "Int" { TINT }
  | "Bool" { TBOOL }
  | "->" { ARROW }
  (* term. *)
  | "true" { BOOL true }
  | "false" { BOOL false }
  | identifier { IDENTIFIER (Lexing.lexeme lexbuf) }
  | int_constant { INT (int_of_string (Lexing.lexeme lexbuf)) }
  (* etc. *)
  | "//" { single_line_comment lexbuf }
  | whitespace { token lexbuf }
  | newline  { next_line lexbuf; token lexbuf }
  | eof { EOF }
  | _ { raise (Failure ("Character not allowed in source text: '" ^ Lexing.lexeme lexbuf ^ "'")) }
and single_line_comment = parse
  | newline { next_line lexbuf; token lexbuf }
  | eof { EOF }
  | _ { single_line_comment lexbuf }
