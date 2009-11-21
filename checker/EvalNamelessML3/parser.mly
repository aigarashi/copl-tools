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
type sobj = DExp of Core.dbexp
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

/* ML3 */
%token FUN RARROW
%token REC

/* NamelessML3 */
%token HASH DOT

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
    Env VDASH DExp EVALTO Val { EvalTo($1, $3, $5) }
  | SInt PLUS SInt IS SInt { AppBOp(Plus, Value_of_int $1, Value_of_int $3, Value_of_int $5) }
  | SInt MULT SInt IS SInt { AppBOp(Mult, Value_of_int $1, Value_of_int $3, Value_of_int $5) }
  | SInt MINUS SInt IS SInt { AppBOp(Minus, Value_of_int $1, Value_of_int $3, Value_of_int $5) }
  | SInt LESS THAN SInt IS TRUE { AppBOp(Lt, Value_of_int $1, Value_of_int $4, Value_of_bool true) }
  | SInt LESS THAN SInt IS FALSE { AppBOp(Lt, Value_of_int $1, Value_of_int $4, Value_of_bool false) }
  /* abbreviations for less than */
  | SInt IS LESS THAN SInt { AppBOp(Lt, Value_of_int $1, Value_of_int $5, Value_of_bool true) }
  | SInt IS NOT LESS THAN SInt { AppBOp(Lt, Value_of_int $1, Value_of_int $6, Value_of_bool false) }

  | Env VDASH DExp error { errAt 4 "Syntax error: 'evalto' expected" }
  | Env VDASH DExp EVALTO error { errAt 5 "Syntax error: value expected" }
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
    Env VDASH DExp EVALTO QM { In_EvalTo($1, $3) }
  | SInt PLUS SInt IS QM { In_AppBOp(Plus, Value_of_int $1, Value_of_int $3) }
  | SInt MULT SInt IS QM { In_AppBOp(Mult, Value_of_int $1, Value_of_int $3) }
  | SInt MINUS SInt IS QM { In_AppBOp(Minus, Value_of_int $1, Value_of_int $3) }
/*  | SInt IS LESS THAN SInt { In_AppBOp(Lt, Value_of_int $1, Value_of_int $5) }
  | SInt IS NOT LESS THAN SInt { AppBOp(Lt, Value_of_int $1, Value_of_int $6) }
*/
  | Env VDASH DExp error { errAt 4 "Syntax error: 'evalto' expected" }
  | Env VDASH DExp EVALTO error { errAt 5 "Syntax error: '?' expected" }
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
  | Val Env2 { List.fold_left (fun env v -> Bind(env, v)) Empty ($1::$2) }

Env2:
    /* empty */ { [] }
  | COMMA Val Env2 { $2 :: $3 }
  | error { errAt 1 "Syntax error: comma expected" }
  | COMMA error { errAt 2 "Syntax error: value expected" }

DExp:
  | LongDExp { $1 }
  | DExp1 { $1 }
  | DExp1 BinOp1 LongDExp { BinOpD($2, $1, $3) } 
  | DExp2 BinOp2 LongDExp { BinOpD($2, $1, $3) } 
  | DExp3 BinOp3 LongDExp { BinOpD($2, $1, $3) } 

  | DExp1 BinOp1 error { errAt 3 "Syntax error: expression expected" }
  | DExp2 BinOp2 error { errAt 3 "Syntax error: expression expected" }
  | DExp3 BinOp3 error { errAt 3 "Syntax error: expression expected" }

LongDExp: 
  | IF DExp THEN DExp ELSE DExp { IfD($2, $4, $6) }
  | LET DOT EQ DExp IN DExp { LetD($4, $6) }
  | LET REC DOT EQ FUN DOT RARROW DExp IN DExp { LetRecD($8, $10) }
  | FUN DOT RARROW DExp { AbsD $4 }

  | IF error { errAt 2 "Syntax error: expression expected" }
  | IF DExp error { errAt 3 "Syntax error: 'then' expected" }
  | IF DExp THEN error { errAt 4 "Syntax error: expression expected" }
  | IF DExp THEN DExp error { errAt 5 "Syntax error: 'else' expected" }
  | IF DExp THEN DExp ELSE error { errAt 6 "Syntax error: expression expected" }
  | LET error { errAt 2 "Syntax error: variable name or 'rec' expected" }
  | LET DOT error { errAt 3 "Syntax error: '=' expected" }
  | LET DOT EQ error { errAt 4 "Syntax error: expression expected" }
  | LET DOT EQ DExp error { errAt 5 "Syntax error: 'in' expected" }
  | LET DOT EQ DExp IN error { errAt 6 "Syntax error: expression expected" }
  | LET REC error { errAt 3 "Syntax error: variable name expected" }
  | LET REC DOT error { errAt 4 "Syntax error: '=' expected" }
  | LET REC DOT EQ error { errAt 5 "Syntax error: 'fun' expected" }
  | LET REC DOT EQ FUN error { errAt 6 "Syntax error: variable name expected" }
  | LET REC DOT EQ FUN DOT { errAt 7 "Syntax error: '->' expected" }
  | LET REC DOT EQ FUN DOT RARROW error { errAt 8 "Syntax error: expression expected" }
  | LET REC DOT EQ FUN DOT RARROW DExp error { errAt 9 "Syntax error: 'in' expected" }
  | LET REC DOT EQ FUN DOT RARROW DExp IN error { errAt 10 "Syntax error: expression expected" }
  | FUN error { errAt 2 "Syntax error: variable name expected" }
  | FUN DOT error { errAt 3 "Syntax error: '->' expected" }
  | FUN DOT RARROW error { errAt 4 "Syntax error: expression expected" }

DExp1:
  | DExp1 BinOp1 DExp2 { BinOpD($2, $1, $3) }
  | DExp2 { $1 }

DExp2:
  | DExp2 BinOp2 DExp3 { BinOpD($2, $1, $3) }
  | DExp3 { $1 }

DExp3:
    DExp3 BinOp3 DExp4 { BinOpD($2, $1, $3) }
  | DExp4 { $1 }

DExp4:  /* function application: 
          argument is an atomic expression without unary minus */
    DExp4 DAExp { AppD($1, $2) }
  | DMinExp { $1 }

BinOp1:
    LT { Lt }

BinOp2:
    CROSS { Plus }
  | HYPHEN { Minus }

BinOp3:
    AST { Mult }

DMinExp: 
    HYPHEN INTL { DBExp_of_int (- $2) }
  | DAExp { $1 }

DAExp:
    INTL { DBExp_of_int $1 }
  | TRUE { DBExp_of_bool true }
  | FALSE { DBExp_of_bool false }
  | HASH INTL { Index $2 }
  | LPAREN DExp RPAREN { $2 }

  | LPAREN error { errAt 2 "Syntax error: expression expected" }
  | LPAREN DExp error { errBtw 1 3 "Syntax error: unmatched parenthesis" }

SInt: /* signed int */
    INTL { $1 }
  | HYPHEN INTL { - $2 }

Val:
    SInt { Value_of_int $1 }
  | TRUE { Value_of_bool true }
  | FALSE { Value_of_bool false }
  | LPAREN Env RPAREN LBRACKET FUN DOT RARROW DExp RBRACKET { Fun($2, $8) }
  | LPAREN Env RPAREN LBRACKET REC DOT EQ FUN DOT RARROW DExp RBRACKET 
      { Rec($2, $11) }

  | LPAREN Env RPAREN error { errAt 4 "Syntax error: '[' expected" }
  | LPAREN Env RPAREN LBRACKET error { errAt 5 "Syntax error: 'fun' or 'rec' expected" }
  | LPAREN Env RPAREN LBRACKET FUN error { errAt 6 "Syntax error: variable expected" }
  | LPAREN Env RPAREN LBRACKET FUN DOT error { errAt 7 "Syntax error: '->' expected" }
  | LPAREN Env RPAREN LBRACKET FUN DOT RARROW error { errAt 8 "Syntax error: expression expected" }
  | LPAREN Env RPAREN LBRACKET FUN DOT RARROW DExp error { errBtw 4 9 "Syntax error: unmatched brackets" }

  | LPAREN Env RPAREN LBRACKET REC error { errAt 6 "Syntax error: variable expected" }
  | LPAREN Env RPAREN LBRACKET REC DOT error { errAt 7 "Syntax error: '=' expected" }
  | LPAREN Env RPAREN LBRACKET REC DOT FUN error { errAt 8 "Syntax error: variable expected" }
  | LPAREN Env RPAREN LBRACKET REC DOT FUN DOT error { errAt 9 "Syntax error: '->' expected" }
  | LPAREN Env RPAREN LBRACKET REC DOT FUN DOT RARROW error { errAt 10 "Syntax error: expression expected" }
  | LPAREN Env RPAREN LBRACKET REC DOT FUN DOT RARROW DExp error { errBtw 4 11 "Syntax error: unmatched brackets" }

/******** experimental feature for macro defintions *********/

MacroDefs: 
    /* empty */ { () }
  | MacroDef MacroDefs { () }

MacroDef:
  | DEF MVEXP EQ DExp SEMI { Hashtbl.add tbl $2 (DExp $4) }
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

DAExp: MVEXP {
  try 
    match Hashtbl.find tbl $1 with
      DExp e -> e
    | _ -> errAt 1 "Cannot happen! DAExp: MVEXP" 
  with Not_found -> errAt 1 ("Undefined macro: " ^ $1)
  }

Env: MVENV {
  try 
    match Hashtbl.find tbl $1 with
      Env e -> e
    | _ -> errAt 1 "Cannot happen! Env: MVENV" 
  with Not_found -> errAt 1 ("Undefined macro: " ^ $1)
  }
