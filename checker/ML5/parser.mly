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

/* common tokens */
%token BY
%token LBRACE RBRACE LPAREN RPAREN LBRACKET RBRACKET
%token SEMI
%token <Derivation.rulename> ID
%token <string> LCID
%token <int> INTL

/* ML1 */
%token PLUS EVALTO MINUS MULT IS LESS THAN NOT
%token AST CROSS HYPHEN LT

%token IF THEN ELSE TRUE FALSE

/* ML2 */
%token VDASH COMMA
%token LET EQ IN 

/* ML3 */
%token FUN RARROW

/* ML4 */
%token REC

/* ML5 */
%token MATCH WITH BAR COLCOL

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
    Judgment BY RName LBRACE RBRACE
    { {conc = $1; by = $3; since = []; pos = rhs_start_pos 3 } }
  | Judgment BY RName LBRACE Derivs
    { {conc = $1; by = $3; since = $5; pos = rhs_start_pos 3 } }
  | Judgment error { errAt 2 "Syntax error: \"by\" expected after a judgment" }
  | Judgment BY RName error { errAt 4 "Syntax error: opening brace expected" }
  | Judgment BY RName LBRACE error { errBtw 4 5 "Syntax error: unmatched brace" }

RName : 
    ID { $1 }
  | LCID { $1 }

Derivs:
  | Derivation RBRACE { [ $1 ] }
  | Derivation SEMI RBRACE { [ $1 ] } 
  | Derivation SEMI Derivs { $1::$3 }
  | Derivation error { errAt 2 "Syntax error: unmatched brace, or semicolon forgotten?" }

Judgment: 
    Env VDASH Exp EVALTO Val { EvalTo($1, $3, $5) }
  | INTL PLUS INTL IS INTL { AppBOp(Plus, Value_of_int $1, Value_of_int $3, Value_of_int $5) }
  | INTL MULT INTL IS INTL { AppBOp(Mult, Value_of_int $1, Value_of_int $3, Value_of_int $5) }
  | INTL MINUS INTL IS INTL { AppBOp(Minus, Value_of_int $1, Value_of_int $3, Value_of_int $5) }
  | INTL LESS THAN INTL IS TRUE { AppBOp(Lt, Value_of_int $1, Value_of_int $4, Value_of_bool true) }
  | INTL LESS THAN INTL IS FALSE { AppBOp(Lt, Value_of_int $1, Value_of_int $4, Value_of_bool false) }
  /* abbreviations for less than */
  | INTL IS LESS THAN INTL { AppBOp(Lt, Value_of_int $1, Value_of_int $5, Value_of_bool true) }
  | INTL IS NOT LESS THAN INTL { AppBOp(Lt, Value_of_int $1, Value_of_int $6, Value_of_bool false) }

  | Env VDASH Exp error { errAt 4 "Syntax error: 'evalto' expected" }
  | Env VDASH Exp EVALTO error { errAt 5 "Syntax error: value expected" }
  | INTL PLUS error { errAt 3 "Syntax error: natural number expected" }
  | INTL PLUS INTL error { errAt 4 "Syntax error: 'is' expected" }
  | INTL PLUS INTL IS error { errAt 5 "Syntax error: natural number expected" }
  | INTL MULT error { errAt 3 "Syntax error: natural number expected" }
  | INTL MULT INTL error { errAt 4 "Syntax error: 'is' expected" }
  | INTL MULT INTL IS error { errAt 5 "Syntax error: natural number expected" }
  | INTL MINUS error { errAt 3 "Syntax error: natural number expected" }
  | INTL MINUS INTL error { errAt 4 "Syntax error: 'is' expected" }
  | INTL MINUS INTL IS error { errAt 5 "Syntax error: natural number expected" }

partialj :
    Env VDASH Exp EVALTO QM { In_EvalTo($1, $3) }
  | INTL PLUS INTL IS QM { In_AppBOp(Plus, Value_of_int $1, Value_of_int $3) }
  | INTL MULT INTL IS QM { In_AppBOp(Mult, Value_of_int $1, Value_of_int $3) }
  | INTL MINUS INTL IS QM { In_AppBOp(Minus, Value_of_int $1, Value_of_int $3) }
/*  | INTL IS LESS THAN INTL { In_AppBOp(Lt, Value_of_int $1, Value_of_int $5) }
  | INTL IS NOT LESS THAN INTL { AppBOp(Lt, Value_of_int $1, Value_of_int $6) }
*/
  | Env VDASH Exp error { errAt 4 "Syntax error: 'evalto' expected" }
  | Env VDASH Exp EVALTO error { errAt 5 "Syntax error: '?' expected" }
  | INTL PLUS error { errAt 3 "Syntax error: natural number expected" }
  | INTL PLUS INTL error { errAt 4 "Syntax error: \'is\' expected" }
  | INTL PLUS INTL IS error { errAt 5 "Syntax error: '?' expected" }
  | INTL MULT error { errAt 3 "Syntax error: natural number expected" }
  | INTL MULT INTL error { errAt 4 "Syntax error: \'is\' expected" }
  | INTL MULT INTL IS error { errAt 5 "Syntax error: '?' expected" }
  | INTL MINUS error { errAt 3 "Syntax error: natural number expected" }
  | INTL MINUS INTL error { errAt 4 "Syntax error: \'is\' expected" }
  | INTL MINUS INTL IS error { errAt 5 "Syntax error: '?' expected" }

Env:
    /* empty */ { Empty } 
  | Env2 LCID EQ Val { Bind($1, $2, $4) }

Env2:
    /* empty */ { Empty } 
  | Env2 LCID EQ Val COMMA { Bind($1, $2, $4) }
  
Exp:
  | LongExp { $1 }
  | Exp1 { $1 }
  | Exp1 BinOp1 LongExp { BinOp($2, $1, $3) } 
  | Exp3 COLCOL LongExp { Cons($1, $3) }  /* left op. of :: is Exp3 (not Exp2) */
  | Exp3 BinOp3 LongExp { BinOp($2, $1, $3) } 
  | Exp4 BinOp4 LongExp { BinOp($2, $1, $3) } 

LongExp: 
  | IF Exp THEN Exp ELSE Exp { If($2, $4, $6) }
  | LET LCID EQ Exp IN Exp { Let($2, $4, $6) }
  | LET REC LCID EQ FUN LCID RARROW Exp IN Exp { LetRec($3, $6, $8, $10) }
  | FUN LCID RARROW Exp { Abs($2, $4) }
  | MATCH Exp WITH LBRACKET RBRACKET RARROW Exp BAR LCID COLCOL LCID RARROW Exp
      { Match($2, $7, $9, $11, $13) }

Exp1:
  | Exp1 BinOp1 Exp2 { BinOp($2, $1, $3) }
  | Exp2 { $1 }

Exp2:
  | Exp3 COLCOL Exp2 { Cons($1, $3) }
  | Exp3 { $1 }

Exp3:
  | Exp3 BinOp3 Exp4 { BinOp($2, $1, $3) }
  | Exp4 { $1 }

Exp4:
    Exp4 BinOp4 Exp5 { BinOp($2, $1, $3) }
  | Exp5 { $1 }

Exp5:  /* function application: 
          argument is an atomic expression without unary minus */
    Exp5 AExp { App($1, $2) }
  | MinExp { $1 }

BinOp1:
    LT { Lt }

BinOp3:
    CROSS { Plus }
  | HYPHEN { Minus }

BinOp4:
    AST { Mult }

MinExp: 
    HYPHEN INTL { Exp_of_int (- $2) }
  | AExp { $1 }

AExp:
    INTL { Exp_of_int $1 }
  | TRUE { Exp_of_bool true }
  | FALSE { Exp_of_bool false }
  | LCID { Exp_of_string $1 }
  | LPAREN Exp RPAREN { $2 }
  | LPAREN Exp error { errBtw 1 3 "Syntax error: unmatched parenthesis" }
  | LBRACKET RBRACKET { Nil }

Val:
    AVal { $1 }
  | AVal COLCOL Val { ConsV($1, $3) }

AVal:
    INTL { Value_of_int $1 }
  | HYPHEN INTL { Value_of_int (- $2) }
  | TRUE { Value_of_bool true }
  | FALSE { Value_of_bool false }
  | LBRACKET RBRACKET { NilV }
  | LPAREN Env RPAREN LBRACKET FUN LCID RARROW Exp RBRACKET { Fun($2, $6, $8) }
  | LPAREN Env RPAREN LBRACKET REC LCID EQ FUN LCID RARROW Exp RBRACKET 
      { Rec($2, $6, $9, $11) }
  | LPAREN Val RPAREN { $2 }



