%{
open Core
open Derivation

let errBtw i j s =
  MySupport.Error.errBtw 
    (Parsing.rhs_start_pos i) (Parsing.rhs_end_pos j) s

let errAt i s =
  MySupport.Error.errAt (Parsing.rhs_start_pos i) s

module S = Set.Make(
  struct type t = string  let compare = Pervasives.compare end
)

exception Not_linear

let rec fpv = function 
    Pat_of_string s -> S.singleton s
  | NilP -> S.empty
  | ConsP (p1, p2) -> 
      let fpv1 = fpv p1 and fpv2 = fpv p2 in
	if S.is_empty (S.inter fpv1 fpv2) then S.union fpv1 fpv2
	else raise Not_linear
  | WildP -> S.empty
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

/* ML6 */
%token UNDERBAR MATCHES DOESNT WHEN

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
  | Judgment BY error { errAt 3 "Syntax error: a rule name expected" }
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
  | Val MATCHES Pat WHEN LPAREN Env RPAREN { Matches($1, $3, Res_of_Env $6) }
  | Val DOESNT MATCH Pat { Matches($1, $4, Fail) }
  | SInt PLUS SInt IS SInt { AppBOp(Plus, Value_of_int $1, Value_of_int $3, Value_of_int $5) }
  | SInt MULT SInt IS SInt { AppBOp(Mult, Value_of_int $1, Value_of_int $3, Value_of_int $5) }
  | SInt MINUS SInt IS SInt { AppBOp(Minus, Value_of_int $1, Value_of_int $3, Value_of_int $5) }
  | SInt LESS THAN SInt IS TRUE { AppBOp(Lt, Value_of_int $1, Value_of_int $4, Value_of_bool true) }
  | SInt LESS THAN SInt IS FALSE { AppBOp(Lt, Value_of_int $1, Value_of_int $4, Value_of_bool false) }
  /* abbreviations for less than */
  | SInt IS LESS THAN SInt { AppBOp(Lt, Value_of_int $1, Value_of_int $5, Value_of_bool true) }
  | SInt IS NOT LESS THAN SInt { AppBOp(Lt, Value_of_int $1, Value_of_int $6, Value_of_bool false) }

  | Env VDASH Exp error { errAt 4 "Syntax error: 'evalto' expected" }
  | Env VDASH Exp EVALTO error { errAt 5 "Syntax error: value expected" }
  | SInt PLUS error { errAt 3 "Syntax error: natural number expected" }
  | SInt PLUS SInt error { errAt 4 "Syntax error: 'is' expected" }
  | SInt PLUS SInt IS error { errAt 5 "Syntax error: natural number expected" }
  | SInt MULT error { errAt 3 "Syntax error: natural number expected" }
  | SInt MULT SInt error { errAt 4 "Syntax error: 'is' expected" }
  | SInt MULT SInt IS error { errAt 5 "Syntax error: natural number expected" }
  | SInt MINUS error { errAt 3 "Syntax error: natural number expected" }
  | SInt MINUS SInt error { errAt 4 "Syntax error: 'is' expected" }
  | SInt MINUS SInt IS error { errAt 5 "Syntax error: natural number expected" }

partialj :
    Env VDASH Exp EVALTO QM { In_EvalTo($1, $3) }
  | Val MATCHES Pat WHEN QM { In_Matches($1, $3) }
  | SInt PLUS SInt IS QM { In_AppBOp(Plus, Value_of_int $1, Value_of_int $3) }
  | SInt MULT SInt IS QM { In_AppBOp(Mult, Value_of_int $1, Value_of_int $3) }
  | SInt MINUS SInt IS QM { In_AppBOp(Minus, Value_of_int $1, Value_of_int $3) }
/*  | SInt IS LESS THAN SInt { In_AppBOp(Lt, Value_of_int $1, Value_of_int $5) }
  | SInt IS NOT LESS THAN SInt { AppBOp(Lt, Value_of_int $1, Value_of_int $6) }
*/
  | Env VDASH Exp error { errAt 4 "Syntax error: 'evalto' expected" }
  | Env VDASH Exp EVALTO error { errAt 5 "Syntax error: '?' expected" }
  | SInt PLUS error { errAt 3 "Syntax error: natural number expected" }
  | SInt PLUS SInt error { errAt 4 "Syntax error: \'is\' expected" }
  | SInt PLUS SInt IS error { errAt 5 "Syntax error: '?' expected" }
  | SInt MULT error { errAt 3 "Syntax error: natural number expected" }
  | SInt MULT SInt error { errAt 4 "Syntax error: \'is\' expected" }
  | SInt MULT SInt IS error { errAt 5 "Syntax error: '?' expected" }
  | SInt MINUS error { errAt 3 "Syntax error: natural number expected" }
  | SInt MINUS SInt error { errAt 4 "Syntax error: \'is\' expected" }
  | SInt MINUS SInt IS error { errAt 5 "Syntax error: '?' expected" }

Env:
    /* empty */ { Empty } 
  | Env2 LCID EQ Val { Bind($1, $2, $4) }
/* error handling */
  | Env2 LCID error { errAt 3 "Syntax error: ':' expected" }
  | Env2 LCID EQ error { errAt 4 "Syntax error: value expected after :" }

Env2:
    /* empty */ { Empty } 
  | Env2 LCID EQ Val COMMA { Bind($1, $2, $4) }
/* error handling */
  | Env2 LCID EQ Val error { errAt 5 "Syntax error: ',' expected" }
  | Env2 LCID EQ error { errAt 4 "Syntax error: value expected after :" }
  | Env2 LCID error { errAt 3 "Syntax error: '=' expected" }

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
  | MATCH Exp WITH Clauses { Match($2, $4) }

NMExp:  
  /* expression which doesn't end with "match": it appears
      outside delimiting contexts */
  | NMLongExp { $1 }
  | Exp1 { $1 }
  | Exp1 BinOp1 NMLongExp { BinOp($2, $1, $3) } 
  | Exp3 COLCOL NMLongExp { Cons($1, $3) }  /* left op. of :: is Exp3 (not Exp2) */
  | Exp3 BinOp3 NMLongExp { BinOp($2, $1, $3) } 
  | Exp4 BinOp4 NMLongExp { BinOp($2, $1, $3) } 

NMLongExp: 
  | IF Exp THEN Exp ELSE NMExp { If($2, $4, $6) }
  | LET LCID EQ Exp IN NMExp { Let($2, $4, $6) }
  | LET REC LCID EQ FUN LCID RARROW Exp IN NMExp { LetRec($3, $6, $8, $10) }
  | FUN LCID RARROW NMExp { Abs($2, $4) }

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

SInt: /* signed int */
    INTL { $1 }
  | HYPHEN INTL { - $2 }

Val:
    AVal { $1 }
  | AVal COLCOL Val { ConsV($1, $3) }

AVal:
    SInt { Value_of_int $1 }
  | TRUE { Value_of_bool true }
  | FALSE { Value_of_bool false }
  | LBRACKET RBRACKET { NilV }
  | LPAREN Env RPAREN LBRACKET FUN LCID RARROW Exp RBRACKET { Fun($2, $6, $8) }
  | LPAREN Env RPAREN LBRACKET REC LCID EQ FUN LCID RARROW Exp RBRACKET 
      { Rec($2, $6, $9, $11) }
  | LPAREN Val RPAREN { $2 }

Clauses: 
  | Pat RARROW Exp { 
	try ignore (fpv $1); AddC($1, $3, EmptyC) with 
	    Not_linear -> errBtw 1 1 "Pattern variables should be disjoint"
      }
  | Pat RARROW NMExp BAR Clauses {
	try ignore (fpv $1); AddC($1, $3, $5) with 
	    Not_linear -> errBtw 1 1 "Pattern variables should be disjoint"
      }

Pat:
    APat { $1 }
  | APat COLCOL Pat { ConsP($1, $3) }
  | APat COLCOL error { errAt 3 "Syntax error: pattern expected" }

APat:
    LCID { Pat_of_string $1 }
  | LBRACKET RBRACKET { NilP }
  | UNDERBAR { WildP }
  | LPAREN Pat RPAREN { $2 }

  | LPAREN Pat error { errBtw 1 3 "Syntax error: closing paren expected" }
