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
%token PLUS MINUS MULT /* EVALTO IS LESS THAN NOT */
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

/* TypingML2 */
%token INT BOOL
%token COLON

/* PolyML4 */
%token PRIME
/* %token ALL */
%token DOT

/* TypingML5 */
%token LIST

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
    Env VDASH Exp COLON Type { Typing($1, $3, $5 []) }
  | Env VDASH Exp error { errAt 4 "Syntax error: colon expected" }
  | Env VDASH Exp COLON error { errAt 5 "Syntax error: type expression expected" }

partialj :
    Env VDASH Exp COLON QM { In_Typing($1, $3) }
  | Env VDASH Exp error { errAt 4 "Syntax error: colon expected" }
  | Env VDASH Exp COLON error { errAt 5 "Syntax error: '?' expected" }

Env:
    /* empty */ { Empty } 
  | Env2 LCID COLON TypeScheme { Bind($1, $2, $4) }
  | Env2 LCID error { errAt 3 "Syntax error: ':' expected" }
  | Env2 LCID COLON error { errAt 4 "Syntax error: type expression expected after :" }

Env2:
    /* empty */ { Empty } 

  | Env2 LCID COLON TypeScheme COMMA { Bind($1, $2, $4) }

  | Env2 LCID COLON Type error { errAt 5 "Syntax error: ',' expected" }
  | Env2 LCID COLON error { errAt 4 "Syntax error: type expression expected after :" }
  | Env2 LCID error { errAt 3 "Syntax error: ':' expected" }
  
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

  /* error handling */
  | IF Exp THEN Exp ELSE error { 
	errAt 6 "Syntax error: expression expected after else" }
  | IF Exp THEN error { errAt 4 "Syntax error: expression expected after then" }
  | IF error { errAt 2 "Syntax error: expression expected after if" }
  | LET LCID EQ Exp IN error { 
	errAt 6 "Syntax error: expression expected after in" }
  | LET LCID EQ error { 
	errAt 4 "Syntax error: expression expected after =" }
  | LET REC LCID EQ FUN LCID RARROW Exp IN error {
	errAt 10 "Syntax error: expression expected after in" }
  | LET REC LCID EQ FUN LCID RARROW error { 
	errAt 8 "Syntax error: expression expected after ->" }
  | LET REC LCID EQ FUN LCID error {
	errAt 7 "Syntax error: '->' expected" }
  | LET REC LCID EQ FUN error { 
	errAt 6 "Syntax error: lowercase identifier expected" }
  | LET REC LCID EQ error { errAt 5 "Syntax error: 'fun' expected" }
  | LET REC LCID error { errAt 4 "Syntax error: '=' expected" }
  | LET REC error { errAt 3 "Syntax error: lowercase identifier expected" }
  | LET error { errAt 2 "Syntax error: lowercase identifier or 'rec' expected after let" }
  | FUN LCID RARROW error { errAt 4 "Syntax error: expression expected" }
  | FUN LCID error { errAt 3 "Syntax error: '->' expected" }
  | FUN error { errAt 2 "Syntax error: lowercase identifier expected" }

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
  | LCID { Exp_of_string $1 }
  | LPAREN Exp RPAREN { $2 }
  | LPAREN Exp error { errBtw 1 3 "Syntax error: unmatched parenthesis" }
  | LBRACKET RBRACKET { Nil }

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
	  with Not_found -> TyFVar $2 
    }
  | PRIME error { errAt 2 "Syntax error: lowercase identifier expected after '" }
  | LPAREN Type RPAREN { $2 }
  | LPAREN Type error { errBtw 1 3 "Syntax error: unmatched parenthesis" }
    
