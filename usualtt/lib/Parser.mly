%{
open Term
%}

%token EOF
%token <string> IDENTIFIER
%token <int> INT
%token <bool> BOOL
// keyword or symbol
%token LET
%token LAM
%token DOT
%token EQ
%token COLON
%token COMMA
%token L_PAREN
%token R_PAREN
// type
%token TINT
%token TBOOL
%token ARROW

%type <Ast.program> program
%start program
%type <Term.term> term
%type <Term.ty> typ

%%

program:
  | term EOF { $1 }
  ;

term:
    // let x : A = t; u
  | LET x=IDENTIFIER COLON a=typ EQ t=term COMMA u=term
    { Let (x, a, t, u) }
    // λ x . t
  | LAM x=IDENTIFIER DOT t=term { Lam (x, t) }
    // t u
  | L_PAREN t=term u=term R_PAREN { App (t, u) }
    // x
  | v=IDENTIFIER { Var v }
    // constants
  | i=INT { Int i }
  | b=BOOL { Bool b }
  ;

typ:
  | TINT { Int }
  | TBOOL { Bool }
  | v=IDENTIFIER { TVar v }
  | t1=typ ARROW t2=typ { Arrow (t1, t2) }
