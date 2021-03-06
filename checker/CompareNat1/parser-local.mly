%{
open Core
open Derivation

let errBtw i j s =
  MySupport.Error.errBtw 
    (Parsing.rhs_start_pos i) (Parsing.rhs_end_pos j) s

let errAt i s =
  MySupport.Error.errBtw (Parsing.rhs_start_pos i) (Parsing.rhs_end_pos i) s
%}

%token EOF

%token BY
%token LBRACE RBRACE LPAREN RPAREN LBRACKET RBRACKET
%token SEMI
%token <Derivation.rulename> ID
%token <string> LCID  /* not used in this game */
%token <string> UCID  /* not used in this game */
%token <int> INTL  /* not used in this game */

%token IS LESS THAN
%token S Z

/******** experimental feature for macro defitinions *********/
%token DEF EQ
%start toplevel partialj judgment
%type <Core.judgment Derivation.t> toplevel
%type <Core.judgment> judgment

%token QM /* stands for question mark to specify holes in a judgment */
%type <Core.in_judgment> partialj

%%

Judgment: 
    Nat IS LESS THAN Nat { Lt($1, $5) }

  | Nat error { errAt 2 "Syntax error: 'is' expected" }
  | Nat IS error { errAt 3 "Syntax error: 'less' expected" }
  | Nat LESS error { errAt 4 "Syntax error: 'than' expected" }
  | Nat IS LESS THAN error { errAt 5 "Syntax error: natural number expected" }

partialj:
    Nat IS LESS THAN Nat { In_Lt($1, $5) }

  | Nat error { errAt 2 "Syntax error: 'is' expected" }
  | Nat IS error { errAt 3 "Syntax error: 'less' expected" }
  | Nat LESS error { errAt 4 "Syntax error: 'than' expected" }
  | Nat IS LESS THAN error { errAt 5 "Syntax error: natural number expected" }

Nat:
    Z { Z }
  | S LPAREN Nat RPAREN { S $3 }
  | S LPAREN Nat error { errBtw 2 4 "Syntax error: unmatched parenthesis" }
  | S LPAREN error { errAt 3 "Syntax error: natural number expected after S(" }
  | S error { errAt 2 "Syntax error: opening parenthesis expected after S" }

