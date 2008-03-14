%{
open Core

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
%token <Core.rulename> ID
%token <int> INTL

%token PLUS EVALTO MINUS MULT IS
%token AST CROSS HYPHEN

%start toplevel
%type <Core.derivation> toplevel

%%

toplevel: 
    Derivation { $1 }

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
    Exp EVALTO INTL { EvalTo($1, Value_of_int $3) }
  | INTL PLUS INTL IS INTL { AppBOp(Plus, Value_of_int $1, Value_of_int $3, Value_of_int $5) }
  | INTL MULT INTL IS INTL { AppBOp(Mult, Value_of_int $1, Value_of_int $3, Value_of_int $5) }
  | INTL MINUS INTL IS INTL { AppBOp(Minus, Value_of_int $1, Value_of_int $3, Value_of_int $5) }

  | Exp EVALTO error { errAt 3 "Syntax error: natural number expected" }
  | INTL PLUS error { errAt 3 "Syntax error: natural number expected" }
  | INTL PLUS INTL error { errAt 4 "Syntax error: \'is\' expected" }
  | INTL PLUS INTL IS error { errAt 5 "Syntax error: natural number expected" }
  | INTL MULT error { errAt 3 "Syntax error: natural number expected" }
  | INTL MULT INTL error { errAt 4 "Syntax error: \'is\' expected" }
  | INTL MULT INTL IS error { errAt 5 "Syntax error: natural number expected" }
  | INTL MINUS error { errAt 3 "Syntax error: natural number expected" }
  | INTL MINUS INTL error { errAt 4 "Syntax error: \'is\' expected" }
  | INTL MINUS INTL IS error { errAt 5 "Syntax error: natural number expected" }

Exp:
    Exp CROSS MExp { BinOp(Plus, $1, $3) }
  | Exp HYPHEN MExp { BinOp(Minus, $1, $3) }
  | MExp { $1 }

MExp:
    MExp AST AExp { BinOp(Mult, $1, $3) }
  | AExp { $1 }

AExp:
    INTL { Exp_of_int $1 }
  | HYPHEN INTL { Exp_of_int (- $2) }
  | LPAREN Exp RPAREN { $2 }
  | LPAREN Exp error { errBtw 1 3 "Syntax error: unmatched parenthesis" }

