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
          | DBExp of Core.dbexp
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
%token HASH DOT DRARROW

/******** experimental feature for macro defitinions *********/
%token DEF EQ
%token <string> MVEXP
%token <string> MVDBEXP
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
  | EOF { raise End_of_file }

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
    Env VDASH Exp DRARROW DExp { TranslateTo($1, $3, $5) }

  | Env VDASH Exp error { errAt 4 "Syntax error: '==>' expected" }
  | Env VDASH Exp DRARROW error { errAt 5 "Syntax error: nameless expression expected" }

partialj :
    Env VDASH Exp DRARROW QM { In_TranslateTo($1, $3) }
  | Env VDASH Exp error { errAt 4 "Syntax error: '==>' expected" }
  | Env VDASH Exp DRARROW error { errAt 5 "Syntax error: '?' expected" }

Env:
    /* empty */ { Empty } 
  | Env2 LCID { Bind($1, Var $2) }

Env2:
    /* empty */ { Empty } 
  | Env2 LCID COMMA { Bind($1, Var $2) }
  
Exp:
  | LongExp { $1 }
  | Exp1 { $1 }
  | Exp1 BinOp1 LongExp { BinOp($2, $1, $3) } 
  | Exp2 BinOp2 LongExp { BinOp($2, $1, $3) } 
  | Exp3 BinOp3 LongExp { BinOp($2, $1, $3) } 

LongExp: 
  | IF Exp THEN Exp ELSE Exp { If($2, $4, $6) }
  | LET LCID EQ Exp IN Exp { Let(Var $2, $4, $6) }
  | LET REC LCID EQ FUN LCID RARROW Exp IN Exp { LetRec(Var $3, Var $6, $8, $10) }
  | FUN LCID RARROW Exp { Abs(Var $2, $4) }

Exp1:
  | Exp1 BinOp1 Exp2 { BinOp($2, $1, $3) }
  | Exp2 { $1 }

Exp2:
  | Exp2 BinOp2 Exp3 { BinOp($2, $1, $3) }
  | Exp3 { $1 }

Exp3:
    Exp3 BinOp3 Exp4 { BinOp($2, $1, $3) }
  | Exp4 { $1 }

Exp4:  /* function application: 
          argument is an atomic expression without unary minus */
    Exp4 AExp { App($1, $2) }
  | MinExp { $1 }

BinOp1:
    LT { Lt }

BinOp2:
    CROSS { Plus }
  | HYPHEN { Minus }

BinOp3:
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
  | LPAREN Exp error { errBtw 1 3 "Syntax error: unmatched parenthesis" }

/* Nameless expressions */
DExp:
  | LongDExp { $1 }
  | DExp1 { $1 }
  | DExp1 BinOp1 LongDExp { BinOpD($2, $1, $3) } 
  | DExp2 BinOp2 LongDExp { BinOpD($2, $1, $3) } 
  | DExp3 BinOp3 LongDExp { BinOpD($2, $1, $3) } 

LongDExp: 
  | IF DExp THEN DExp ELSE DExp { IfD($2, $4, $6) }
  | LET DOT EQ DExp IN DExp { LetD($4, $6) }
  | LET REC DOT EQ FUN DOT RARROW DExp IN DExp { LetRecD($8, $10) }
  | FUN DOT RARROW DExp { AbsD $4 }

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

DMinExp: 
    HYPHEN INTL { DBExp_of_int (- $2) }
  | DAExp { $1 }

DAExp:
    INTL { DBExp_of_int $1 }
  | TRUE { DBExp_of_bool true }
  | FALSE { DBExp_of_bool false }
  | HASH INTL { Index $2 }
  | LPAREN DExp RPAREN { $2 }
  | LPAREN DExp error { errBtw 1 3 "Syntax error: unmatched parenthesis" }


/******** experimental feature for macro defintions *********/

MacroDefs: 
    /* empty */ { () }
  | MacroDef MacroDefs { () }

MacroDef:
  | DEF MVEXP EQ Exp SEMI { Hashtbl.add tbl $2 (Exp $4) }
  | DEF MVDBEXP EQ DExp SEMI { Hashtbl.add tbl $2 (DBExp $4) }
  | DEF MVENV EQ Env SEMI { Hashtbl.add tbl $2 (Env $4) }

AExp: MVEXP {
  try 
    match Hashtbl.find tbl $1 with
      Exp e -> e
    | _ -> errAt 1 "Cannot happen! AExp: MVEXP" 
  with Not_found -> errAt 1 ("Undefined macro: " ^ $1)
  }

DAExp: MVDBEXP {
  try 
    match Hashtbl.find tbl $1 with
      DBExp e -> e
    | _ -> errAt 1 "Cannot happen! DAExp: MVDBEXP" 
  with Not_found -> errAt 1 ("Undefined macro: " ^ $1)
  }

Env: MVENV {
  try 
    match Hashtbl.find tbl $1 with
      Env e -> e
    | _ -> errAt 1 "Cannot happen! Env: MVENV" 
  with Not_found -> errAt 1 ("Undefined macro: " ^ $1)
  }
