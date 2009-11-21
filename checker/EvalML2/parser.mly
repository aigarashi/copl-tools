%{
open Core
open Derivation

let errBtw i j s =
  MySupport.Error.errBtw 
    (Parsing.rhs_start_pos i) (Parsing.rhs_end_pos j) s

let errAt i s =
  MySupport.Error.errAt (Parsing.rhs_start_pos i) s

(******** experimental feature for macro defitinions *********)
(* The following definition could be automatically generated from .gm *)
type sobj = Exp of Core.exp
	  | Value of Core.value
	  | Env of Core.env

let tbl = Hashtbl.create 1024
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

/******** experimental feature for macro defitinions *********/
%token DEF EQ
%token <string> MVEXP
%token <string> MVVALUE
%token <string> MVENV

%start toplevel partialj judgment
%type <Core.judgment Derivation.t> toplevel
%type <Core.judgment> judgment

%token QM /* stands for question mark to specify holes in a judgment */
%type <Core.in_judgment> partialj

%%

toplevel: 
    MacroDefs Derivation { $2 }
  | MacroDefs error { errAt 2 "Syntax error: derivation expected" }
  | EOF { raise End_of_file }

judgment: Judgment { $1 }

Derivation: 
    Judgment BY RName LBRACE RBRACE
    { {conc = $1; by = $3; since = []; pos = rhs_start_pos 3 } }
  | Judgment BY RName LBRACE Derivs
    { {conc = $1; by = $3; since = $5; pos = rhs_start_pos 3 } }
  | Judgment error { errAt 2 "Syntax error: 'by' expected after a judgment" }
  | Judgment BY error { errAt 3 "Syntax error: rule name expected after 'by'" }
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
  | SInt PLUS SInt IS QM { In_AppBOp(Plus, Value_of_int $1, Value_of_int $3) }
  | SInt MULT SInt IS QM { In_AppBOp(Mult, Value_of_int $1, Value_of_int $3) }
  | SInt MINUS SInt IS QM { In_AppBOp(Minus, Value_of_int $1, Value_of_int $3) }
/*  | SInt IS LESS THAN SInt { In_AppBOp(Lt, Value_of_int $1, Value_of_int $5) }
  | SInt IS NOT LESS THAN SInt { AppBOp(Lt, Value_of_int $1, Value_of_int $6) }
*/
  | Env VDASH Exp error { errAt 4 "Syntax error: 'evalto' expected" }
  | Env VDASH Exp EVALTO error { errAt 5 "Syntax error: '?' expected" }
  | SInt PLUS error { errAt 3 "Syntax error: natural number expected" }
  | SInt PLUS SInt error { errAt 4 "Syntax error: 'is' expected" }
  | SInt PLUS SInt IS error { errAt 5 "Syntax error: '?' expected" }
  | SInt MULT error { errAt 3 "Syntax error: natural number expected" }
  | SInt MULT SInt error { errAt 4 "Syntax error: 'is' expected" }
  | SInt MULT SInt IS error { errAt 5 "Syntax error: '?' expected" }
  | SInt MINUS error { errAt 3 "Syntax error: natural number expected" }
  | SInt MINUS SInt error { errAt 4 "Syntax error: 'is' expected" }
  | SInt MINUS SInt IS error { errAt 5 "Syntax error: '?' expected" }

Env:
    /* empty */ { Empty } 
  | Env2 LCID EQ Val { Bind($1, Var $2, $4) }
  | Env2 LCID error { errAt 3 "Syntax error: '=' expected" }
  | Env2 LCID EQ error { errAt 4 "Syntax error: value expected" }

Env2:
    /* empty */ { Empty } 
  | Env2 LCID EQ Val COMMA { Bind($1, Var $2, $4) }
  | Env2 LCID error { errAt 3 "Syntax error: '=' expected" }
  | Env2 LCID EQ error { errAt 4 "Syntax error: value expected" }
  | Env2 LCID EQ Val error { errAt 5 "Syntax error: ',' expected" }
  
Exp:
  | LongExp { $1 }
  | Exp1 { $1 }
  | Exp1 BinOp1 LongExp { BinOp($2, $1, $3) } 
  | Exp2 BinOp2 LongExp { BinOp($2, $1, $3) } 
  | Exp3 BinOp3 LongExp { BinOp($2, $1, $3) } 

  | Exp1 BinOp1 error { errAt 3 "Syntax error: expression expected" }
  | Exp2 BinOp2 error { errAt 3 "Syntax error: expression expected" }
  | Exp3 BinOp3 error { errAt 3 "Syntax error: expression expected" }

LongExp: 
  | IF Exp THEN Exp ELSE Exp { If($2, $4, $6) }
  | LET LCID EQ Exp IN Exp { Let(Var $2, $4, $6) }

  | IF error { errAt 2 "Syntax error: expression expected" }
  | IF Exp error { errAt 3 "Syntax error: 'then' expected" }
  | IF Exp THEN error { errAt 4 "Syntax error: expression expected" }
  | IF Exp THEN Exp error { errAt 5 "Syntax error: 'else' expected" }
  | IF Exp THEN Exp ELSE error { errAt 6 "Syntax error: expression expected" }
  | LET error { errAt 2 "Syntax error: variable name expected" }
  | LET LCID error { errAt 3 "Syntax error: '=' expected" }
  | LET LCID EQ error { errAt 4 "Syntax error: expression expected" }
  | LET LCID EQ Exp error { errAt 5 "Syntax error: 'in' expected" }
  | LET LCID EQ Exp IN error { errAt 6 "Syntax error: expression expected" }

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
    SInt { Exp_of_int $1 }
  | TRUE { Exp_of_bool true }
  | FALSE { Exp_of_bool false }
  | LCID { Exp_of_Var (Var $1) }
  | LPAREN Exp RPAREN { $2 }

  | LPAREN error { errAt 2 "Syntax error: expression expected" }
  | LPAREN Exp error { errBtw 1 3 "Syntax error: unmatched parenthesis" }

SInt: /* signed int */
    INTL { $1 }
  | HYPHEN INTL { - $2 }

Val:
    SInt { Value_of_int $1 }
  | TRUE { Value_of_bool true }
  | FALSE { Value_of_bool false }

/******** experimental feature for macro defintions *********/

MacroDefs: 
    /* empty */ { () }
  | MacroDef MacroDefs { () }

MacroDef:
  | DEF MVEXP EQ Exp SEMI { Hashtbl.add tbl $2 (Exp $4) }
  | DEF MVVALUE EQ Val SEMI { Hashtbl.add tbl $2 (Value $4) }
  | DEF MVENV EQ Env SEMI { Hashtbl.add tbl $2 (Env $4) }

  | DEF MVEXP EQ error { errAt 4 "Syntax error: expression expected" }
  | DEF MVVALUE EQ error { errAt 4 "Syntax error: value expected" }
  | DEF MVENV EQ error { errAt 4 "Syntax error: environment expected" }
  | DEF error { errAt 2 "Syntax error: metavariable (with $) expected" }

Val: MVVALUE { 
  try
    match Hashtbl.find tbl $1 with 
      Value v -> v
    | _ -> errAt 1 "Cannot happen! Val: MVVALUE" 
  with Not_found -> errAt 1 ("Undefined macro: " ^ $1)
}

AExp: MVEXP {
  try 
    match Hashtbl.find tbl $1 with
      Exp e -> e
    | _ -> errAt 1 "Cannot happen! AExp: MVEXP" 
  with Not_found -> errAt 1 ("Undefined macro: " ^ $1)
  }

Env: MVENV {
  try 
    match Hashtbl.find tbl $1 with
      Env e -> e
    | _ -> errAt 1 "Cannot happen! Env: MVENV" 
  with Not_found -> errAt 1 ("Undefined macro: " ^ $1)
  }

Env2: MVENV COMMA {
  try 
    match Hashtbl.find tbl $1 with
      Env e -> e
    | _ -> errAt 1 "Cannot happen! Env: MVENV" 
  with Not_found -> errAt 1 ("Undefined macro: " ^ $1)
  }
