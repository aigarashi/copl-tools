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
	  | Store of Core.store

let tbl = Hashtbl.create 1024
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
%token REC

/* RefML3 */
%token BANG COLONEQ REF AT SLASH

/******** experimental feature for macro defitinions *********/
%token DEF EQ
%token <string> MVEXP
%token <string> MVVALUE
%token <string> MVENV
%token <string> MVSTORE

%start toplevel partialj judgment
%type <Core.judgment Derivation.t> toplevel
%type <Core.judgment> judgment

%token QM /* stands for question mark to specify holes in a judgment */
%type <Core.in_judgment> partialj

%%

SLASHStore :
    /* empty */ { EmptyS }
  | SLASH Store { $2 }

  | SLASH error { errAt 2 "Syntax error: store expected" }

Judgment: 
    Store SLASH Env VDASH Exp EVALTO Val SLASHStore { EvalTo($1, $3, $5, $7, $8) }
  | Env VDASH Exp EVALTO Val SLASHStore { EvalTo(EmptyS, $1, $3, $5, $6) }
  | SInt PLUS SInt IS SInt { AppBOp(Plus, Value_of_int $1, Value_of_int $3, Value_of_int $5) }
  | SInt MULT SInt IS SInt { AppBOp(Mult, Value_of_int $1, Value_of_int $3, Value_of_int $5) }
  | SInt MINUS SInt IS SInt { AppBOp(Minus, Value_of_int $1, Value_of_int $3, Value_of_int $5) }
  | SInt LESS THAN SInt IS TRUE { AppBOp(Lt, Value_of_int $1, Value_of_int $4, Value_of_bool true) }
  | SInt LESS THAN SInt IS FALSE { AppBOp(Lt, Value_of_int $1, Value_of_int $4, Value_of_bool false) }
  /* abbreviations for less than */
  | SInt IS LESS THAN SInt { AppBOp(Lt, Value_of_int $1, Value_of_int $5, Value_of_bool true) }
  | SInt IS NOT LESS THAN SInt { AppBOp(Lt, Value_of_int $1, Value_of_int $6, Value_of_bool false) }

  | Store SLASH error { errAt 3 "Syntax error: environment expected" }
  | Store SLASH Env VDASH error { errAt 5 "Syntax error: expression expected" }
  | Store SLASH Env VDASH Exp error { errAt 6 "Syntax error: 'evalto' expected" }
  | Store SLASH Env VDASH Exp EVALTO error { errAt 7 "Syntax error: value expected" }

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
    Store SLASH Env VDASH Exp EVALTO QM { In_EvalTo($1, $3, $5) }
  | Env VDASH Exp EVALTO QM { In_EvalTo(EmptyS, $1, $3) }
  | SInt PLUS SInt IS QM { In_AppBOp(Plus, Value_of_int $1, Value_of_int $3) }
  | SInt MULT SInt IS QM { In_AppBOp(Mult, Value_of_int $1, Value_of_int $3) }
  | SInt MINUS SInt IS QM { In_AppBOp(Minus, Value_of_int $1, Value_of_int $3) }
/*  | SInt IS LESS THAN SInt { In_AppBOp(Lt, Value_of_int $1, Value_of_int $5) }
  | SInt IS NOT LESS THAN SInt { AppBOp(Lt, Value_of_int $1, Value_of_int $6) }
*/
  | Env VDASH Exp error { errAt 4 "Syntax error: 'evalto' expected" }
  | Env VDASH Exp EVALTO error { errAt 5 "Syntax error: '?' expected" }
  | Store SLASH error { errAt 3 "Syntax error: environment expected" }
  | Store SLASH Env VDASH Exp error { errAt 6 "Syntax error: 'evalto' expected" }
  | Store SLASH Env VDASH Exp EVALTO error { errAt 7 "Syntax error: '?' expected" }
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

Store:
    /* empty  { EmptyS } */
  | AT LCID EQ Val Store2 { List.fold_left (fun store (id, v) -> Block(store, Loc id, v)) EmptyS (($2,$4)::$5) }
  | AT error { errAt 2 "Syntax error: location name expected" }
  | AT LCID error { errAt 3 "Syntax error: '=' expected" }
  | AT LCID EQ error { errAt 4 "Syntax error: value expected" }

Store2:
    /* empty */ { [] } 
  | COMMA AT LCID EQ Val Store2 { ($3, $5) :: $6 }
/*  | error { errAt 1 "Syntax error: comma expected" }  */
  | COMMA error { errAt 2 "Syntax error: '@' expected" }
  | COMMA AT error { errAt 3 "Syntax error: location name expected" }
  | COMMA AT LCID error { errAt 4 "Syntax error: '=' expected" }
  | COMMA AT LCID EQ error { errAt 5 "Syntax error: value expected" }

Exp:
  | LongExp { $1 }
  | Exp1 { $1 }
  | Exp2 COLONEQ LongExp { Assign($1, $3) }  /* left op. of := is Exp2 (not Exp1) */
  | Exp2 BinOp2 LongExp { BinOp($2, $1, $3) } 
  | Exp3 BinOp3 LongExp { BinOp($2, $1, $3) } 
  | Exp4 BinOp4 LongExp { BinOp($2, $1, $3) } 

  | Exp2 COLONEQ error { errAt 3 "Syntax error: expression expected" }
  | Exp2 BinOp2 error { errAt 3 "Syntax error: expression expected" }
  | Exp3 BinOp3 error { errAt 3 "Syntax error: expression expected" }
  | Exp4 BinOp4 error { errAt 3 "Syntax error: expression expected" }

LongExp: 
  | IF Exp THEN Exp ELSE Exp { If($2, $4, $6) }
  | LET LCID EQ Exp IN Exp { Let(Var $2, $4, $6) }
  | LET REC LCID EQ FUN LCID RARROW Exp IN Exp { LetRec(Var $3, Var $6, $8, $10) }
  | FUN LCID RARROW Exp { Abs(Var $2, $4) }

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

Exp1:
  | Exp2 COLONEQ Exp1 { Assign($1, $3) }
  | Exp2 { $1 }

Exp2:
  | Exp2 BinOp2 Exp3 { BinOp($2, $1, $3) }
  | Exp3 { $1 }

Exp3:
    Exp3 BinOp3 Exp4 { BinOp($2, $1, $3) }
  | Exp4 { $1 }

Exp4:
    Exp4 BinOp4 Exp5 { BinOp($2, $1, $3) }
  | Exp5 { $1 }

Exp5:  /* function application: 
          argument is an atomic expression without unary minus */
    Exp5 AExp { App($1, $2) }
  | REF AExp { NewRef($2) }
  | MinExp { $1 }

BinOp2:
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
  | BANG AExp { Deref($2) }
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
  | AT LCID { Value_of_Loc (Loc $2) }
  | LPAREN Env RPAREN LBRACKET FUN LCID RARROW Exp RBRACKET
      { Fun($2, Var $6, $8) }
  | LPAREN Env RPAREN LBRACKET REC LCID EQ FUN LCID RARROW Exp RBRACKET 
      { Rec($2, Var $6, Var $9, $11) }

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


/******** experimental feature for macro defintions *********/

MacroDefs: 
  | MacroDef MacroDefs { () }

MacroDef:
  | DEF MVEXP EQ Exp SEMI { Hashtbl.add tbl $2 (Exp $4) }
  | DEF MVVALUE EQ Val SEMI { Hashtbl.add tbl $2 (Value $4) }
  | DEF MVENV EQ Env SEMI { Hashtbl.add tbl $2 (Env $4) }
  | DEF MVSTORE EQ Store SEMI { Hashtbl.add tbl $2 (Store $4) }

  | DEF MVEXP EQ error { errAt 4 "Syntax error: expression expected" }
  | DEF MVVALUE EQ error { errAt 4 "Syntax error: value expected" }
  | DEF MVENV EQ error { errAt 4 "Syntax error: environment expected" }
  | DEF MVSTORE EQ error { errAt 4 "Syntax error: store expected" }
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

Store: MVSTORE Store2 {
  try 
    match Hashtbl.find tbl $1 with
      Store s -> List.fold_left (fun store (id, v) -> Block(store, Loc id, v)) s $2
    | _ -> errAt 1 "Cannot happen! Store: MVSTORE" 
  with Not_found -> errAt 1 ("Undefined macro: " ^ $1)
  }
