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
	  | Type of Core.types
	  | Env of Core.env
	  | Sig of (Core.sg -> Core.sg)

let tbl = Hashtbl.create 1024

module S = Set.Make(
  struct type t = string  let compare = Pervasives.compare end
)

exception Not_linear

let rec fpv = function 
    Pat_of_Var (Var s) -> S.singleton s
  | CnstrP _ -> S.empty
  | CnstrPi(_, p) -> fpv p
  | CnstrPii(_, p1, p2) -> 
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

%token PLUS /* EVALTO */ MINUS MULT /* IS LESS THAN NOT */
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

/* TypingML4 */
%token INT BOOL
%token COLON
%token LIST

/* TypingML6 */
%token UNDERBAR
%token TYPE OF
%token MATCHES WHEN

/******** experimental feature for macro defitinions *********/
%token DEF EQ
%token <string> MVEXP
%token <string> MVTYPE
%token <string> MVTENV
%token <string> MVSIG

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
    Sig SEMI Env VDASH Exp COLON Type { Typing($1 EmptyS, $3, $5, $7) }
  | Env VDASH Exp COLON Type { Typing(EmptyS, $1, $3, $5) }
  | Sig SEMI Type MATCHES Pat WHEN LPAREN Env RPAREN { PatTyping($1 EmptyS, $3, $5, $8) }

  | Sig SEMI Env VDASH Exp error { errAt 6 "Syntax error: colon expected" }
  | Sig SEMI Env VDASH Exp COLON error { errAt 7 "Syntax error: type expression expected" }
  | Env VDASH Exp error { errAt 4 "Syntax error: colon expected" }
  | Env VDASH Exp COLON error { errAt 5 "Syntax error: type expression expected" }
  | Sig SEMI Type MATCHES error { errAt 7 "Syntax error: pattern expected" }
  | Sig SEMI Type MATCHES Pat WHEN error { errAt 7 "Syntax error: '(' expected" }


partialj :
    Sig SEMI Env VDASH Exp COLON QM { In_Typing($1 EmptyS, $3, $5) }
  | Sig SEMI Env VDASH Exp error { errAt 6 "Syntax error: colon expected" }
  | Sig SEMI Env VDASH Exp COLON error { errAt 7 "Syntax error: '?' expected" }
  | Env VDASH Exp COLON QM { In_Typing(EmptyS, $1, $3) }
  | Env VDASH Exp error { errAt 4 "Syntax error: colon expected" }
  | Env VDASH Exp COLON error { errAt 5 "Syntax error: '?' expected" }

Sig:
    /* empty */ { fun sg -> sg }
  | TypeDecl Sig { fun sg -> $2 ($1 sg) }

TypeDecl:
    TYPE LCID EQ BarOpt UCID ArgTypesOpt CnstrDeclSeq { 
      fun sg -> $7 (TyName $2) ($6 (TyName $2) (Cnstr $5) sg)
    }
  | TYPE error { errAt 2 "Syntax error: type name expected" }
  | TYPE LCID error { errAt 3 "Syntax error: '=' expected" }
  | TYPE LCID EQ error { errAt 4 "Syntax error: constructor name expected" }
  | TYPE LCID EQ BarOpt UCID error { errAt 6 "Syntax error: type or '|' expected" } 

BarOpt:
    /* empty */ { () }
  | BAR { () }

ArgTypesOpt:
    /* empty */ { fun cod cnstr sg -> 
	BindS(sg, cnstr, CnstrT cod) 
    }
  | OF AType     { fun cod cnstr sg -> 
	BindS(sg, cnstr, CnstrTi($2, cod))
    }
  | OF AType AST AType { fun cod cnstr sg ->
        BindS(sg, cnstr, CnstrTii($2, $4, cod))
    }

  | OF error { errAt 2 "Syntax error: type expected" }
  | OF AType AST error { errAt 4 "Syntax error: type expected" }

CnstrDeclSeq:
    /* empty */ { fun cod sg -> sg }
  | BAR UCID ArgTypesOpt CnstrDeclSeq { fun cod sg -> $4 cod ($3 cod (Cnstr $2) sg) }

  | BAR error { errAt 2 "Syntax error: constructor name expected" }
  | BAR UCID error { errAt 3 "Syntax error: type of '|' expected" }

Env:
    /* empty */ { Empty } 
  | LCID COLON Type Env2 { List.fold_left (fun env (id, v) -> Bind(env, Var id, v)) Empty (($1,$3)::$4) }
  | LCID error { errAt 2 "Syntax error: ':' expected" }
  | LCID COLON error { errAt 3 "Syntax error: type expected" }

Env2:
    /* empty */ { [] } 
  | COMMA LCID COLON Type Env2 { ($2, $4) :: $5 }
  | error { errAt 1 "Syntax error: comma expected" }
  | COMMA error { errAt 2 "Syntax error: variable expected" }
  | COMMA LCID error { errAt 3 "Syntax error: ':' expected" }
  | COMMA LCID COLON error { errAt 4 "Syntax error: type expected" }

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
  | Exp2 BinOp2 NMLongExp { BinOp($2, $1, $3) } 
  | Exp3 BinOp3 NMLongExp { BinOp($2, $1, $3) } 

NMLongExp: 
  | IF Exp THEN Exp ELSE NMExp { If($2, $4, $6) }
  | LET LCID EQ Exp IN NMExp { Let(Var $2, $4, $6) }
  | LET REC LCID EQ FUN LCID RARROW Exp IN NMExp { LetRec(Var $3, Var $6, $8, $10) }
  | FUN LCID RARROW NMExp { Abs(Var $2, $4) }

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
    Exp4 AExp { 
      match $1 with
	  CnstrE c -> CnstrEi(c, $2)
	| _ -> App($1, $2) 
    }  
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
  | UCID { CnstrE (Cnstr $1) }
  | UCID LPAREN Exp RPAREN { CnstrEi(Cnstr $1, $3) }
  | UCID LPAREN Exp COMMA Exp RPAREN { CnstrEii(Cnstr $1, $3, $5) }
  | LPAREN Exp RPAREN { $2 }

  | UCID LPAREN error { errAt 3 "Syntax error: expression expected" }
  | UCID LPAREN Exp error { errBtw 2 4 "Syntax error: unmatched parenthesis or a comma expected" }
  | UCID LPAREN Exp COMMA error { errAt 5 "Syntax error: expression expected" }
  | UCID LPAREN Exp COMMA Exp error { errBtw 2 6 "Syntax error: unmatched parenthesis" }
  | LPAREN error { errAt 2 "Syntax error: expression expected" }
  | LPAREN Exp error { errBtw 1 3 "Syntax error: unmatched parenthesis" }

Clauses: 
  | Pat RARROW Exp { 
	try ignore (fpv $1); AddC($1, $3, EmptyC) with 
	    Not_linear -> errBtw 1 1 "Pattern variables should be disjoint"
      }
  | Pat RARROW NMExp BAR Clauses {
	try ignore (fpv $1); AddC($1, $3, $5) with 
	    Not_linear -> errBtw 1 1 "Pattern variables should be disjoint"
      }

  | Pat error { errAt 2 "Syntax error: '->' expected" }

APat:
    LCID { Pat_of_Var (Var $1) }
  | UNDERBAR { WildP }
  | UCID { CnstrP(Cnstr $1) }

Pat:
    LCID { Pat_of_Var (Var $1) }
  | UNDERBAR { WildP }
  | UCID { CnstrP(Cnstr $1) }
  | UCID APat { CnstrPi(Cnstr $1, $2) }
  | UCID LPAREN Pat COMMA Pat RPAREN { CnstrPii(Cnstr $1, $3, $5) }

Type:
    AType { $1 }
  | AType RARROW Type { TyFun($1, $3) }
  | AType RARROW error { errAt 3 "Syntax error: type expected after ->" }

AType:
    INT { TyInt }
  | BOOL { TyBool }
  | LCID { Types_of_TyName (TyName $1) }
  | LPAREN Type RPAREN { $2 }
  | LPAREN Type error { errBtw 1 3 "Syntax error: unmatched parenthesis" }
    
/******** experimental feature for macro defintions *********/

MacroDefs: 
    /* empty */ { () }
  | MacroDef MacroDefs { () }

MacroDef:
  | DEF MVEXP EQ Exp SEMI { Hashtbl.add tbl $2 (Exp $4) }
  | DEF MVTYPE EQ Type SEMI { Hashtbl.add tbl $2 (Type $4) }
  | DEF MVTENV EQ Env SEMI { Hashtbl.add tbl $2 (Env $4) }
  | DEF MVSIG EQ Sig SEMI { Hashtbl.add tbl $2 (Sig $4) } 

  | DEF MVEXP EQ error { errAt 4 "Syntax error: expression expected" }
  | DEF MVTYPE EQ error { errAt 4 "Syntax error: type expected" }
  | DEF MVTENV EQ error { errAt 4 "Syntax error: environment expected" }
  | DEF MVSIG EQ error { errAt 4 "Syntax error: data type definitions expected" }
  | DEF error { errAt 2 "Syntax error: metavariable (with $) expected" }

Type: MVTYPE { 
  try
    match Hashtbl.find tbl $1 with 
      Type v -> v
    | _ -> errAt 1 "Cannot happen! Type: MVTYPE" 
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

Sig: MVSIG {
  try 
    match Hashtbl.find tbl $1 with
      Sig sg -> sg
    | _ -> errAt 1 "Cannot happen! Sig: MVSIG" 
  with Not_found -> errAt 1 ("Undefined macro: " ^ $1)
}

