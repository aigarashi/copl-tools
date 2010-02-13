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
	  | Type of (string list -> Core.types)
	  | TySc of Core.tyscheme
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
%token PLUS MINUS MULT /* EVALTO IS LESS THAN NOT */
%token AST CROSS HYPHEN LT

%token IF THEN ELSE TRUE FALSE

/* ML2 */
%token VDASH COMMA
%token LET EQ IN 

/* ML3 */
%token FUN RARROW
%token REC

/* ML4 */
%token MATCH WITH BAR COLCOL

/* TypingML2 */
%token INT BOOL
%token COLON

/* TypingML4 */
%token LIST

/* PolyTypingML4 */
%token PRIME
/* %token ALL */
%token DOT

/******** experimental feature for macro defitinions *********/
%token DEF EQ
%token <string> MVEXP
%token <string> MVTYPE
%token <string> MVTYSC
%token <string> MVTENV

%start toplevel partialj judgment
%type <Core.judgment Derivation.t> toplevel
%type <Core.judgment> judgment

%token QM /* stands for question mark to specify holes in a judgment */
%type <Core.in_judgment> partialj

%%

toplevel: 
    MacroDefs Derivation { $2 }
  | error { errAt 1 "Syntax error, perhaps at the beginning of the input" }
  | EOF { raise End_of_file }

judgment: Judgment { $1 }

Derivation: 
    Judgment BY RName LBRACE RBRACE
    { {conc = $1; by = $3; since = []; pos = rhs_start_pos 3 } }
  | Judgment BY RName LBRACE Derivs
    { {conc = $1; by = $3; since = $5; pos = rhs_start_pos 3 } }
  | Judgment error { errAt 2 "Syntax error: \"by\" expected after a judgment" }
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
    Env VDASH Exp COLON Type { Typing($1, $3, $5 []) }
  | Env VDASH Exp error { errAt 4 "Syntax error: colon expected" }
  | Env VDASH Exp COLON error { errAt 5 "Syntax error: type expression expected" }

partialj :
    Env VDASH Exp COLON QM { In_Typing($1, $3) }
  | Env VDASH Exp error { errAt 4 "Syntax error: colon expected" }
  | Env VDASH Exp COLON error { errAt 5 "Syntax error: '?' expected" }

Env:
    /* empty */ { Empty } 
  | LCID COLON TypeScheme Env2 { List.fold_left (fun env (id, v) -> Bind(env, Var id, v)) Empty (($1,$3)::$4) }
  | LCID error { errAt 2 "Syntax error: ':' expected" }
  | LCID COLON error { errAt 3 "Syntax error: type expected" }

Env2:
    /* empty */ { [] } 
  | COMMA LCID COLON TypeScheme Env2 { ($2, $4) :: $5 }
  | error { errAt 1 "Syntax error: comma expected" }
  | COMMA error { errAt 2 "Syntax error: variable expected" }
  | COMMA LCID error { errAt 3 "Syntax error: ':' expected" }
  | COMMA LCID COLON error { errAt 4 "Syntax error: type expected" }

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
  | MATCH Exp WITH LBRACKET RBRACKET RARROW Exp BAR LCID COLCOL LCID RARROW Exp
      { Match($2, $7, Var $9, Var $11, $13) }

  /* error handling */
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
  | MATCH Exp WITH error { errAt 4 "Syntax error: '[]' expected" }
  | MATCH Exp WITH LBRACKET error { errAt 5 "Syntax error: ']' expected" }
  | MATCH Exp WITH LBRACKET RBRACKET error { errAt 6 "Syntax error: '->' expected" }
  | MATCH Exp WITH LBRACKET RBRACKET RARROW error { errAt 7 "Syntax error: expression expected" }
  | MATCH Exp WITH LBRACKET RBRACKET RARROW Exp error { errAt 8 "Syntax error: '|' expected" }
  | MATCH Exp WITH LBRACKET RBRACKET RARROW Exp BAR error { errAt 9 "Syntax error: variable expected" }
  | MATCH Exp WITH LBRACKET RBRACKET RARROW Exp BAR LCID error { errAt 10 "Syntax error: '::' expected" }
  | MATCH Exp WITH LBRACKET RBRACKET RARROW Exp BAR LCID COLCOL error
      { errAt 11 "Syntax error: variable expected" }
  | MATCH Exp WITH LBRACKET RBRACKET RARROW Exp BAR LCID COLCOL LCID error
      { errAt 12 "Syntax error: '->' expected" }
  | MATCH Exp WITH LBRACKET RBRACKET RARROW Exp BAR LCID COLCOL LCID RARROW error
      { errAt 13 "Syntax error: expression expected" }


Exp1:
  | Exp1 BinOp1 Exp2 { BinOp($2, $1, $3) }
  | Exp2 { $1 }

Exp2:
  | Exp3 COLCOL Exp2 { Cons($1, $3) }
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

TyVarDecls:
    PRIME LCID { [$2] }
  | PRIME LCID /* COMMA */ TyVarDecls { $2 :: $3 }

TypeScheme:
    Type { TyScheme_of_Types ($1 []) }
/*  | ALL LPAREN TyVarDecls RPAREN LBRACKET Type RBRACKET */
  | TyVarDecls DOT Type { 
	let i = List.length $1 in
        TyScheme(i, $3 $1)
    }

Type:
    Type2 { $1 }
  | Type2 RARROW Type { fun ids -> TyFun($1 ids, $3 ids) }
  | Type2 RARROW error { errAt 3 "Syntax error: type expected after ->" }

Type2:
    AType { $1 }
  | Type2 LIST { fun ids -> TyList ($1 ids) }

AType:
    INT { fun _ -> TyInt }
  | BOOL { fun _ -> TyBool }
  | PRIME LCID { 
	fun ids -> 
	  try TyBVar(MySupport.Pervasives.pos $2 ids) 
	  with Not_found -> TyFVar (TVar $2) 
    }
  | PRIME error { errAt 2 "Syntax error: lowercase identifier expected after '" }
  | LPAREN Type RPAREN { $2 }
  | LPAREN Type error { errBtw 1 3 "Syntax error: unmatched parenthesis" }
    
/******** experimental feature for macro defintions *********/

MacroDefs: 
    /* empty */ { () }
  | MacroDef MacroDefs { () }

MacroDef:
  | DEF MVEXP EQ Exp SEMI { Hashtbl.add tbl $2 (Exp $4) }
  | DEF MVTYPE EQ Type SEMI { Hashtbl.add tbl $2 (Type $4) }
  | DEF MVTYSC EQ TypeScheme SEMI { Hashtbl.add tbl $2 (TySc $4) }
  | DEF MVTENV EQ Env SEMI { Hashtbl.add tbl $2 (Env $4) }

  | DEF MVEXP EQ error { errAt 4 "Syntax error: expression expected" }
  | DEF MVTYPE EQ error { errAt 4 "Syntax error: type expected" }
  | DEF MVTYSC EQ error { errAt 4 "Syntax error: type scheme expected" }
  | DEF MVTENV EQ error { errAt 4 "Syntax error: environment expected" }
  | DEF error { errAt 2 "Syntax error: metavariable (with $) expected" }

Type: MVTYPE { 
  fun ids -> 
  try
    match Hashtbl.find tbl $1 with 
      Type v -> v ids
    | _ -> errAt 1 "Cannot happen! Type: MVTYPE" 
  with Not_found -> errAt 1 ("Undefined macro: " ^ $1)
}

TypeScheme: MVTYSC { 
  try
    match Hashtbl.find tbl $1 with 
      TySc v -> v
    | _ -> errAt 1 "Cannot happen! Type: MVTYSC" 
  with Not_found -> errAt 1 ("Undefined macro: " ^ $1)
}

AExp: MVEXP {
  try 
    match Hashtbl.find tbl $1 with
      Exp e -> e
    | _ -> errAt 1 "Cannot happen! AExp: MVEXP" 
  with Not_found -> errAt 1 ("Undefined macro: " ^ $1)
  }

Env: MVTENV Env2 {
  try 
    match Hashtbl.find tbl $1 with
      Env e -> List.fold_left (fun env (id, v) -> Bind(env, Var id, v)) e $2
    | _ -> errAt 1 "Cannot happen! Env: MVTENV" 
  with Not_found -> errAt 1 ("Undefined macro: " ^ $1)
  }
