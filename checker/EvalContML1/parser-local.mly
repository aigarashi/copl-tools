%{
open Core
open Derivation

let errBtw i j s =
  MySupport.Error.errBtw 
    (Parsing.rhs_start_pos i) (Parsing.rhs_end_pos j) s

let errAt i s =
  MySupport.Error.errBtw (Parsing.rhs_start_pos i) (Parsing.rhs_end_pos i) s

(******** experimental feature for macro defitinions *********)
(* The following definition could be automatically generated from .gm *)
type sobj = Exp of Core.exp
	  | Value of Core.value
	  | Cont of Core.cont

let tbl = Hashtbl.create 1024
%}

%token EOF

/* common tokens */
%token BY
%token LBRACE RBRACE LPAREN RPAREN LBRACKET RBRACKET
%token SEMI
%token <Derivation.rulename> ID
%token <string> LCID  /* not used in this game */
%token <string> UCID  /* not used in this game */
%token <int> INTL

/* ML1 */
%token PLUS EVALTO MINUS MULT IS LESS THAN NOT
%token AST CROSS HYPHEN LT

%token IF THEN ELSE TRUE FALSE

/* ContML1 */
%token GTGT UNDERSCORE DRARROW

/******** experimental feature for macro defitinions *********/
%token DEF EQ
%token <string> MVEXP
%token <string> MVVALUE
%token <string> MVCONT

%start toplevel partialj judgment
%type <Core.judgment Derivation.t> toplevel
%type <Core.judgment> judgment

%token QM /* stands for question mark to specify holes in a judgment */
%type <Core.in_judgment> partialj

%%

Judgment: 
    Exp GTGT Cont EVALTO Val { EvalTo($3, $1, $5) }
  | Exp EVALTO Val { EvalTo(RetK, $1, $3) }
  | Val DRARROW Cont EVALTO Val { AppK($3, $1, $5) }

  | SInt PLUS SInt IS SInt { AppBOp(Plus, Value_of_int $1, Value_of_int $3, Value_of_int $5) }
  | SInt MULT SInt IS SInt { AppBOp(Mult, Value_of_int $1, Value_of_int $3, Value_of_int $5) }
  | SInt MINUS SInt IS SInt { AppBOp(Minus, Value_of_int $1, Value_of_int $3, Value_of_int $5) }
  | SInt LESS THAN SInt IS TRUE { AppBOp(Lt, Value_of_int $1, Value_of_int $4, Value_of_bool true) }
  | SInt LESS THAN SInt IS FALSE { AppBOp(Lt, Value_of_int $1, Value_of_int $4, Value_of_bool false) }
  /* abbreviations for less than */
  | SInt IS LESS THAN SInt { AppBOp(Lt, Value_of_int $1, Value_of_int $5, Value_of_bool true) }
  | SInt IS NOT LESS THAN SInt { AppBOp(Lt, Value_of_int $1, Value_of_int $6, Value_of_bool false) }

  | Exp error { errAt 2 "Syntax error: 'evalto' expected" }
  | Exp EVALTO error { errAt 3 "Syntax error: value expected" }
  | Exp GTGT error { errAt 3 "Syntax error: continuation expected" }
  | Exp GTGT Cont error { errAt 4 "Syntax error: 'evalto' expected" }
  | Exp GTGT Cont EVALTO error { errAt 5 "Syntax error: value expected" }

  | Val DRARROW error { errAt 3 "Syntax error: continuation expected" }
  | Val DRARROW Cont error { errAt 4 "Syntax error: 'evalto' expected" }
  | Val DRARROW Cont EVALTO error { errAt 5 "Syntax error: value expected" }

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
    Exp GTGT Cont EVALTO QM { In_EvalTo($3, $1) }
  | Exp EVALTO QM { In_EvalTo(RetK, $1) }
  | Val DRARROW Cont EVALTO QM { In_AppK($3, $1) }
  | SInt PLUS SInt IS QM { In_AppBOp(Plus, Value_of_int $1, Value_of_int $3) }
  | SInt MULT SInt IS QM { In_AppBOp(Mult, Value_of_int $1, Value_of_int $3) }
  | SInt MINUS SInt IS QM { In_AppBOp(Minus, Value_of_int $1, Value_of_int $3) }
/*  | SInt IS LESS THAN SInt { In_AppBOp(Lt, Value_of_int $1, Value_of_int $5) }
  | SInt IS NOT LESS THAN SInt { AppBOp(Lt, Value_of_int $1, Value_of_int $6) }
*/
  | Exp error { errAt 2 "Syntax error: 'evalto' expected" }
  | Exp EVALTO error { errAt 3 "Syntax error: '?' expected" }
  | Exp GTGT error { errAt 3 "Syntax error: continuation expected" }
  | Exp GTGT Cont error { errAt 4 "Syntax error: 'evalto' expected" }
  | Exp GTGT Cont EVALTO error { errAt 5 "Syntax error: '?' expected" }
  | SInt PLUS error { errAt 3 "Syntax error: natural number expected" }
  | SInt PLUS SInt error { errAt 4 "Syntax error: 'is' expected" }
  | SInt PLUS SInt IS error { errAt 5 "Syntax error: '?' expected" }
  | SInt MULT error { errAt 3 "Syntax error: natural number expected" }
  | SInt MULT SInt error { errAt 4 "Syntax error: 'is' expected" }
  | SInt MULT SInt IS error { errAt 5 "Syntax error: '?' expected" }
  | SInt MINUS error { errAt 3 "Syntax error: natural number expected" }
  | SInt MINUS SInt error { errAt 4 "Syntax error: 'is' expected" }
  | SInt MINUS SInt IS error { errAt 5 "Syntax error: '?' expected" }

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
    SInt { Exp_of_int $1 }
  | TRUE { Exp_of_bool true }
  | FALSE { Exp_of_bool false }
  | LPAREN Exp RPAREN { $2 }
  | LPAREN Exp error { errBtw 1 3 "Syntax error: unmatched parenthesis" }

SInt: /* signed int */
    INTL { $1 }
  | HYPHEN INTL { - $2 }

Val:
    SInt { Value_of_int $1 }
  | TRUE { Value_of_bool true }
  | FALSE { Value_of_bool false }

BinOp: BinOp1 {$1} | BinOp2 {$1} | BinOp3 {$1} 

Hole:  UNDERSCORE { RetK }

OptCont: GTGT Cont { $2 }
  | /* empty */ { RetK }
  | GTGT error { errAt 2 "Syntax error: continuation expected" }
Cont:
  | Hole { RetK }
  | LBRACE Hole BinOp1 Exp2 RBRACE OptCont 
     { EvalRK($4, $3, $6) }
  | LBRACE Hole BinOp2 Exp3 RBRACE OptCont 
     { EvalRK($4, $3, $6) }
  | LBRACE Hole BinOp3 AExp RBRACE OptCont
     { EvalRK($4, $3, $6) }
  | LBRACE Hole BinOp LongExp RBRACE OptCont 
     { EvalRK($4, $3, $6) }
  | LBRACE Val BinOp Hole RBRACE OptCont { AppOpK($2, $3, $6) }
  | LBRACE IF Hole THEN Exp ELSE Exp RBRACE OptCont
     { BranchK($5, $7, $9) }

  | LBRACE error { errAt 2 "Syntax error: '_' or value or 'if' expected" }
  | LBRACE Hole error { errAt 3 "Syntax error: '+', '-', '*', or '<' expected" }
  | LBRACE Hole BinOp1 error { errAt 4 "Syntax error: expression expected" }
  | LBRACE Hole BinOp1 Exp2 error { errAt 5 "Syntax error: '}' expected" }
  | LBRACE Hole BinOp2 error { errAt 4 "Syntax error: expression expected" }
  | LBRACE Hole BinOp2 Exp3 error { errAt 5 "Syntax error: '}' expected" }
  | LBRACE Hole BinOp3 error { errAt 4 "Syntax error: expression expected" }
  | LBRACE Hole BinOp3 AExp error { errAt 5 "Syntax error: '}' expected" }
  | LBRACE Hole BinOp LongExp error { errAt 5 "Syntax error: '}' expected" }
  | LBRACE Val error { errAt 3 "Syntax error: '+', '-', '*', or '<' expected" }
  | LBRACE Val BinOp error { errAt 4 "Syntax error: '_' expected" }
  | LBRACE Val BinOp Hole error { errAt 5 "Syntax error: '}' expected" }
  | LBRACE IF error { errAt 3 "Syntax error: '_' expected" }
  | LBRACE IF Hole error { errAt 4 "Syntax error: 'then' expected" }
  | LBRACE IF Hole THEN error { errAt 5 "Syntax error: expression expected" }
  | LBRACE IF Hole THEN Exp error { errAt 6 "Syntax error: 'else' expected" }
  | LBRACE IF Hole THEN Exp ELSE error { errAt 7 "Syntax error: expression expected" }
  | LBRACE IF Hole THEN Exp ELSE Exp error { errAt 8 "Syntax error: '}' expected" }


/******** experimental feature for macro defintions *********/

MacroDefs: 
  | MacroDef MacroDefs { () }

MacroDef:
  | DEF MVEXP EQ Exp SEMI { Hashtbl.add tbl $2 (Exp $4) }
  | DEF MVVALUE EQ Val SEMI { Hashtbl.add tbl $2 (Value $4) }
  | DEF MVCONT EQ Cont SEMI { Hashtbl.add tbl $2 (Cont $4) }

  | DEF MVEXP EQ error { errAt 4 "Syntax error: expression expected" }
  | DEF MVVALUE EQ error { errAt 4 "Syntax error: value expected" }
  | DEF MVCONT EQ error { errAt 4 "Syntax error: continuation expected"  }
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

Cont: MVCONT {
  try 
    match Hashtbl.find tbl $1 with
      Cont e -> e
    | _ -> errAt 1 "Cannot happen! Cont: MVCONT" 
  with Not_found -> errAt 1 ("Undefined macro: " ^ $1)
  }
