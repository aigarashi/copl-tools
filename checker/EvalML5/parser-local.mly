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
	  | Env of Core.env

let tbl = Hashtbl.create 1024

module S = Set.Make(
  struct type t = string  let compare = Pervasives.compare end
)

exception Not_linear

let rec fpv = function 
    Pat_of_Var (Var s) -> S.singleton s
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
%token <string> UCID  /* not used in this game */
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

Judgment: 
    Env VDASH Exp EVALTO Val { EvalTo($1, $3, $5) }
  | Pat MATCHES Val WHEN LPAREN Env RPAREN { Matches($3, $1, Res_of_Env $6) }
  | Pat DOESNT MATCH Val { Matches($4, $1, Fail) }
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
  | Pat MATCHES Val WHEN QM { In_Matches($3, $1) }
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
  | LCID EQ Val Env2 { List.fold_left (fun env (id, v) -> Bind(env, Var id, v)) Empty (($1,$3)::$4) }
  | LCID error { errAt 2 "Syntax error: '=' expected" }
  | LCID EQ error { errAt 3 "Syntax error: value expected" }

Env2:
    /* empty */ { [] }
  | COMMA LCID EQ Val Env2 { ($2, $4) :: $5 }
  | error { errAt 1 "Syntax error: comma expected" }
  | COMMA error { errAt 2 "Syntax error: variable expected" }
  | COMMA LCID error { errAt 3 "Syntax error: '=' expected" }
  | COMMA LCID EQ error { errAt 4 "Syntax error: value expected" }
  
Exp:
  | LongExp { $1 }
  | Exp1 { $1 }
  | Exp1 BinOp1 LongExp { BinOp($2, $1, $3) } 
  | Exp3 COLCOL LongExp { Cons($1, $3) }  /* left op. of :: is Exp3 (not Exp2) */
  | Exp3 BinOp3 LongExp { BinOp($2, $1, $3) } 
  | Exp4 BinOp4 LongExp { BinOp($2, $1, $3) } 

  | Exp1 BinOp1 error { errAt 3 "Syntax error: expression expected" }
  | Exp3 COLCOL error { errAt 3 "Syntax error: expression expected" }
  | Exp3 BinOp3 error { errAt 3 "Syntax error: expression expected" }
  | Exp4 BinOp4 error { errAt 3 "Syntax error: expression expected" }

LongExp: 
  | IF Exp THEN Exp ELSE Exp { If($2, $4, $6) }
  | LET LCID EQ Exp IN Exp { Let(Var $2, $4, $6) }
  | LET REC LCID EQ FUN LCID RARROW Exp IN Exp { LetRec(Var $3, Var $6, $8, $10) }
  | FUN LCID RARROW Exp { Abs(Var $2, $4) }
  | MATCH Exp WITH Clauses { Match($2, $4) }

  | IF error { errAt 2 "Syntax error: expression expected" }
  | IF Exp error { errAt 3 "Syntax error: 'then' expected" }
  | IF Exp THEN error { errAt 4 "Syntax error: expression expected" }
  | IF Exp THEN Exp error { errAt 5 "Syntax error: 'else' expected" }
  | IF Exp THEN Exp ELSE error { errAt 6 "Syntax error: expression expected" }
  | LET error { errAt 2 "Syntax error: variable name or 'rec' expected" }
  | LET LCID error { errAt 3 "Syntax error: '=' expected" }
  | LET LCID EQ error { errAt 4 "Syntax error: expression expected" }
  | LET LCID EQ Exp error { errAt 5 "Syntax error: 'in' expected" }
  | LET LCID EQ Exp IN error { errAt 6 "Syntax error: expression expected" }
  | LET REC error { errAt 3 "Syntax error: variable name expected" }
  | LET REC LCID error { errAt 4 "Syntax error: '=' expected" }
  | LET REC LCID EQ error { errAt 5 "Syntax error: 'fun' expected" }
  | LET REC LCID EQ FUN error { errAt 6 "Syntax error: variable name expected" }
  | LET REC LCID EQ FUN LCID { errAt 7 "Syntax error: '->' expected" }
  | LET REC LCID EQ FUN LCID RARROW error { errAt 8 "Syntax error: expression expected" }
  | LET REC LCID EQ FUN LCID RARROW Exp error { errAt 9 "Syntax error: 'in' expected" }
  | LET REC LCID EQ FUN LCID RARROW Exp IN error { errAt 10 "Syntax error: expression expected" }
  | FUN error { errAt 2 "Syntax error: variable name expected" }
  | FUN LCID error { errAt 3 "Syntax error: '->' expected" }
  | FUN LCID RARROW error { errAt 4 "Syntax error: expression expected" }
  | MATCH error { errAt 2 "Syntax error: expression expected" }
  | MATCH Exp error { errAt 3 "Syntax error: 'with' expected" }
  | MATCH Exp WITH error { errAt 4 "Syntax error: pattern expected" }

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
  | LET LCID EQ Exp IN NMExp { Let(Var $2, $4, $6) }
  | LET REC LCID EQ FUN LCID RARROW Exp IN NMExp { LetRec(Var $3, Var $6, $8, $10) }
  | FUN LCID RARROW NMExp { Abs(Var $2, $4) }

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
  | LCID { Exp_of_Var (Var $1) }
  | LPAREN Exp RPAREN { $2 }
  | LBRACKET RBRACKET { Nil }

  | LPAREN error { errAt 2 "Syntax error: expression expected" }
  | LPAREN Exp error { errBtw 1 3 "Syntax error: unmatched parenthesis" }
  | LBRACKET error { errAt 2 "Syntax error: ']' expected" }

SInt: /* signed int */
    INTL { $1 }
  | HYPHEN INTL { - $2 }

Val:
    AVal { $1 }
  | AVal COLCOL Val { ConsV($1, $3) }

  | AVal COLCOL error { errAt 3 "Syntax error: value expected" }

AVal:
    SInt { Value_of_int $1 }
  | TRUE { Value_of_bool true }
  | FALSE { Value_of_bool false }
  | LBRACKET RBRACKET { NilV }
  | LPAREN Env RPAREN LBRACKET FUN LCID RARROW Exp RBRACKET { Fun($2, Var $6, $8) }
  | LPAREN Env RPAREN LBRACKET REC LCID EQ FUN LCID RARROW Exp RBRACKET 
      { Rec($2, Var $6, Var $9, $11) }
  | LPAREN Val RPAREN { $2 }

  | LBRACKET error { errAt 2 "Syntax error: ']' expected" }
  | LPAREN Env RPAREN error { errAt 4 "Syntax error: '[' expected" }
  | LPAREN Env RPAREN LBRACKET error { errAt 5 "Syntax error: 'fun' or 'rec' expected" }
  | LPAREN Env RPAREN LBRACKET FUN error { errAt 6 "Syntax error: variable expected" }
  | LPAREN Env RPAREN LBRACKET FUN LCID error { errAt 7 "Syntax error: '->' expected" }
  | LPAREN Env RPAREN LBRACKET FUN LCID RARROW error { errAt 8 "Syntax error: expression expected" }
  | LPAREN Env RPAREN LBRACKET FUN LCID RARROW Exp error { errBtw 4 9 "Syntax error: unmatched brackets" }

  | LPAREN Env RPAREN LBRACKET REC error { errAt 6 "Syntax error: variable expected" }
  | LPAREN Env RPAREN LBRACKET REC LCID error { errAt 7 "Syntax error: '=' expected" }
  | LPAREN Env RPAREN LBRACKET REC LCID FUN error { errAt 8 "Syntax error: variable expected" }
  | LPAREN Env RPAREN LBRACKET REC LCID FUN LCID error { errAt 9 "Syntax error: '->' expected" }
  | LPAREN Env RPAREN LBRACKET REC LCID FUN LCID RARROW error { errAt 10 "Syntax error: expression expected" }
  | LPAREN Env RPAREN LBRACKET REC LCID FUN LCID RARROW Exp error { errBtw 4 11 "Syntax error: unmatched brackets" }
  | LPAREN Val error { errBtw 1 3 "Syntax error: unmatched parenthesis" }


Clauses: 
  | Pat RARROW Exp { 
	try ignore (fpv $1); SingleC($1, $3) with 
	    Not_linear -> errBtw 1 1 "Pattern variables should be disjoint"
      }
  | Pat RARROW NMExp BAR Clauses {
	try ignore (fpv $1); AddC($1, $3, $5) with 
	    Not_linear -> errBtw 1 1 "Pattern variables should be disjoint"
      }

  | Pat error { errAt 2 "Syntax error: '->' expected" }

Pat:
    APat { $1 }
  | APat COLCOL Pat { ConsP($1, $3) }
  | APat COLCOL error { errAt 3 "Syntax error: pattern expected" }

APat:
    LCID { Pat_of_Var (Var $1) }
  | LBRACKET RBRACKET { NilP }
  | UNDERBAR { WildP }
  | LPAREN Pat RPAREN { $2 }

  | LBRACKET error { errAt 2 "Syntax error: ']' expected" }
  | LPAREN Pat error { errBtw 1 3 "Syntax error: closing paren expected" }

/******** experimental feature for macro defintions *********/

MacroDefs: 
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

Env: MVENV Env2 {
  try 
    match Hashtbl.find tbl $1 with
      Env e -> List.fold_left (fun env (id, v) -> Bind(env, Var id, v)) e $2
    | _ -> errAt 1 "Cannot happen! Env: MVENV" 
  with Not_found -> errAt 1 ("Undefined macro: " ^ $1)
  }
