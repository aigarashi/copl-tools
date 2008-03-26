%{
open Core
open Derivation

let errBtw i j s =
  MySupport.Error.errBtw 
    (Parsing.rhs_start_pos i) (Parsing.rhs_end_pos j) s

let errAt i s =
  MySupport.Error.errAt (Parsing.rhs_start_pos i) s
%}

%token EOF

%token BY
%token LBRACE RBRACE LPAREN RPAREN
%token SEMI
%token <Derivation.rulename> ID
%token <int> INTL  /* not used in this game */

%token PLUS EVALTO MULT IS
%token AST CROSS S Z

%start toplevel
%type <Core.judgment Derivation.t> toplevel

%%

toplevel: 
    Derivation { $1 }
  | EOF { exit 0 }

Derivation: 
    Judgment BY ID LBRACE RBRACE
    { {conc = $1; by = $3; since = []; pos = rhs_start_pos 3 } }
  | Judgment BY ID LBRACE Derivs
    { {conc = $1; by = $3; since = $5; pos = rhs_start_pos 3 } }
  | Judgment error { errAt 2 "Syntax error: \"by\" expected after a judgment" }
  | Judgment BY ID error { errAt 4 "Syntax error: opening brace expected" }
  | Judgment BY ID LBRACE error { errBtw 4 5 "Syntax error: unmatched brace" }

Derivs:
  | Derivation RBRACE { [ $1 ] }
  | Derivation SEMI RBRACE { [ $1 ] } 
  | Derivation SEMI Derivs { $1::$3 }
  | Derivation error { errAt 2 "Syntax error: unmatched brace, or semicolon forgotten?" }

Judgment: 
    Exp EVALTO Nat { EvalTo($1, $3) }
  | Nat PLUS Nat IS Nat { PlusIs($1, $3, $5) }
  | Nat MULT Nat IS Nat { MultIs($1, $3, $5) }

  | Exp EVALTO error { errAt 3 "Syntax error: natural number expected" }
  | Nat PLUS error { errAt 3 "Syntax error: natural number expected" }
  | Nat PLUS Nat error { errAt 4 "Syntax error: \'is\' expected" }
  | Nat PLUS Nat IS error { errAt 5 "Syntax error: natural number expected" }
  | Nat MULT error { errAt 3 "Syntax error: natural number expected" }
  | Nat MULT Nat error { errAt 4 "Syntax error: \'is\' expected" }
  | Nat MULT Nat IS error { errAt 5 "Syntax error: natural number expected" }

Exp:
    Exp CROSS MExp { P($1, $3) }
  | MExp { $1 }

MExp:
    MExp AST AExp { M($1, $3) }
  | AExp { $1 }

AExp:
    Nat { Exp_of_Nat $1 }
  | LPAREN Exp RPAREN { $2 }
  | LPAREN Exp error { errBtw 1 3 "Syntax error: unmatched parenthesis" }

Nat:
    Z { Z }
  | S LPAREN Nat RPAREN { S $3 }
  | S LPAREN Nat error { errBtw 2 4 "Syntax error: unmatched parenthesis" }
  | S error { errAt 2 "Syntax error: opening parenthesis expected after S" }

