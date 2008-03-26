%{
open Checker
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
%token <int> INTL

%token PLUS EVALTO MINUS MULT IS LESS THAN NOT
%token AST CROSS HYPHEN LT

%token IF THEN ELSE TRUE FALSE

%start toplevel
%type <Checker.judgment Derivation.t> toplevel

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
    Exp EVALTO Val { EvalTo($1, $3) }
  | INTL PLUS INTL IS INTL { AppBOp(Plus, Value_of_int $1, Value_of_int $3, Value_of_int $5) }
  | INTL MULT INTL IS INTL { AppBOp(Mult, Value_of_int $1, Value_of_int $3, Value_of_int $5) }
  | INTL MINUS INTL IS INTL { AppBOp(Minus, Value_of_int $1, Value_of_int $3, Value_of_int $5) }
  | INTL IS LESS THAN INTL { AppBOp(Lt, Value_of_int $1, Value_of_int $5, Value_of_Boolean True) }
  | INTL IS NOT LESS THAN INTL { AppBOp(Lt, Value_of_int $1, Value_of_int $6, Value_of_Boolean False) }

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
  | LongExp { $1 }
  | Exp1 { $1 }
  | Exp1 BinOp1 LongExp { BinOp($2, $1, $3) } 
  | Exp2 BinOp2 LongExp { BinOp($2, $1, $3) } 
  | Exp3 BinOp3 LongExp { BinOp($2, $1, $3) } 

LongExp: 
  | IF Exp THEN Exp ELSE Exp { If($2, $4, $6) }

Exp1:
  | Exp1 BinOp1 Exp2 { BinOp($2, $1, $3) }
  | Exp2 { $1 }

Exp2:
  | Exp2 BinOp2 Exp3 { BinOp($2, $1, $3) }
  | Exp3 { $1 }

Exp3:
    Exp3 BinOp3 AExp { BinOp($2, $1, $3) }
  | AExp { $1 }

BinOp1:
    LT { Lt }

BinOp2:
    CROSS { Plus }
  | HYPHEN { Minus }

BinOp3:
    AST { Mult }

AExp:
    INTL { Exp_of_int $1 }
  | HYPHEN INTL { Exp_of_int (- $2) }
  | TRUE { Exp_of_Boolean True }
  | FALSE { Exp_of_Boolean False }
  | LPAREN Exp RPAREN { $2 }
  | LPAREN Exp error { errBtw 1 3 "Syntax error: unmatched parenthesis" }

Val:
    INTL { Value_of_int $1 }
  | TRUE { Value_of_Boolean True }
  | FALSE { Value_of_Boolean False }
