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
%token LBRACE RBRACE LPAREN RPAREN LBRACKET RBRACKET
%token SEMI
%token <Derivation.rulename> ID
%token <string> LCID  /* not used in this game */
%token <int> INTL  /* not used in this game */

%token PLUS MULT IS
%token S Z

%token DEF EQ

/******** experimental feature for macro defitinions *********/
%token DEF EQ
%start toplevel partialj judgment
%type <Core.judgment Derivation.t> toplevel
%type <Core.judgment> judgment

%token QM /* stands for question mark to specify holes in a judgment */
%type <Core.in_judgment> partialj

%%

toplevel: 
    Derivation { $1 }
  | EOF { exit 0 } 

judgment: Judgment { $1 }

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
  | Nat PLUS Nat IS Nat { PlusIs($1, $3, $5) }
  | Nat MULT Nat IS Nat { MultIs($1, $3, $5) }

  | Nat PLUS error { errAt 3 "Syntax error: natural number expected" }
  | Nat PLUS Nat error { errAt 4 "Syntax error: \'is\' expected" }
  | Nat PLUS Nat IS error { errAt 5 "Syntax error: natural number expected" }
  | Nat MULT error { errAt 3 "Syntax error: natural number expected" }
  | Nat MULT Nat error { errAt 4 "Syntax error: \'is\' expected" }
  | Nat MULT Nat IS error { errAt 5 "Syntax error: natural number expected" }

partialj:
  | Nat PLUS Nat IS QM { In_PlusIs($1, $3) }
  | Nat MULT Nat IS QM { In_MultIs($1, $3) }

  | Nat PLUS error { errAt 3 "Syntax error: natural number expected" }
  | Nat PLUS Nat error { errAt 4 "Syntax error: \'is\' expected" }
  | Nat PLUS Nat IS error { errAt 5 "Syntax error: '?' expected" }
  | Nat MULT error { errAt 3 "Syntax error: natural number expected" }
  | Nat MULT Nat error { errAt 4 "Syntax error: \'is\' expected" }
  | Nat MULT Nat IS error { errAt 5 "Syntax error: '?' expected" }

Nat:
    Z { Z }
  | S LPAREN Nat RPAREN { S $3 }
  | S LPAREN Nat error { errBtw 2 4 "Syntax error: unmatched parenthesis" }
  | S error { errAt 2 "Syntax error: opening parenthesis expected after S" }


