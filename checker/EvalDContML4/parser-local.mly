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
	  | Cont of Core.cont
	  | MCont of Core.mcont

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

/* ML4 */
%token MATCH WITH BAR COLCOL
/* ContML4 */
%token GTGT DRARROW UNDERSCORE

/* DContML4 */
%token SHIFT GTGTGT USCOREUSCORE

/******** experimental feature for macro defitinions *********/
%token DEF EQ
%token <string> MVEXP
%token <string> MVVALUE
%token <string> MVENV
%token <string> MVCONT
%token <string> MVMCONT


%start toplevel partialj judgment
%type <Core.judgment Derivation.t> toplevel
%type <Core.judgment> judgment

%token QM /* stands for question mark to specify holes in a judgment */
%type <Core.in_judgment> partialj

%%

Judgment:
    Env VDASH Exp GTGT Cont GTGTGT MCont EVALTO Val { EvalTo($1, $5, $7, $3, $9) }
  | Env VDASH Exp GTGT Cont EVALTO Val { EvalTo($1, $5, RetKK, $3, $7) }
  | Env VDASH Exp EVALTO Val { EvalTo($1, RetK, RetKK, $3, $5) }
  | Val DRARROW Cont GTGTGT MCont EVALTO Val { AppK($3, $5, $1, $7) }
  | Val DRARROW Cont EVALTO Val { AppK($3, RetKK, $1, $5) }

  | SInt PLUS SInt IS SInt { AppBOp(Plus, Value_of_int $1, Value_of_int $3, Value_of_int $5) }
  | SInt MULT SInt IS SInt { AppBOp(Mult, Value_of_int $1, Value_of_int $3, Value_of_int $5) }
  | SInt MINUS SInt IS SInt { AppBOp(Minus, Value_of_int $1, Value_of_int $3, Value_of_int $5) }
  | SInt LESS THAN SInt IS TRUE { AppBOp(Lt, Value_of_int $1, Value_of_int $4, Value_of_bool true) }
  | SInt LESS THAN SInt IS FALSE { AppBOp(Lt, Value_of_int $1, Value_of_int $4, Value_of_bool false) }
  /* abbreviations for less than */
  | SInt IS LESS THAN SInt { AppBOp(Lt, Value_of_int $1, Value_of_int $5, Value_of_bool true) }
  | SInt IS NOT LESS THAN SInt { AppBOp(Lt, Value_of_int $1, Value_of_int $6, Value_of_bool false) }

  | Env VDASH Exp error { errAt 4 "Syntax error: 'evalto' or '>>' expected" }
  | Env VDASH Exp EVALTO error { errAt 5 "Syntax error: value expected" }
  | Env VDASH Exp GTGT error { errAt 5 "Syntax error: continuation expected" }
  | Env VDASH Exp GTGT Cont error { errAt 6 "Syntax error: 'evalto' or '>>>' expected" }
  | Env VDASH Exp GTGT Cont EVALTO error { errAt 7 "Syntax error: value expected" }
  | Env VDASH Exp GTGT Cont GTGTGT error { errAt 7 "Syntax error: meta-continuation expected" }
  | Env VDASH Exp GTGT Cont GTGTGT MCont error { errAt 8 "Syntax error: 'evalto' expected" }
  | Env VDASH Exp GTGT Cont GTGTGT MCont EVALTO error { errAt 9 "Syntax error: value expected" }
  | Val DRARROW error { errAt 3 "Syntax error: continuation expected" }
  | Val DRARROW Cont error { errAt 4 "Syntax error: 'evalto' or '>>>' expected" }
  | Val DRARROW Cont EVALTO error { errAt 5 "Syntax error: value expected" }
  | Val DRARROW Cont GTGTGT error { errAt 5 "Syntax error: meta-continuation expected" }
  | Val DRARROW Cont GTGTGT MCont error { errAt 6 "Syntax error: 'evalto' expected" }
  | Val DRARROW Cont GTGTGT MCont EVALTO error { errAt 7 "Syntax error: value expcted" }

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
    Env VDASH Exp EVALTO QM { In_EvalTo($1, RetK, RetKK, $3) }
  | Env VDASH Exp GTGT Cont EVALTO QM { In_EvalTo($1, $5, RetKK, $3) }
  | Env VDASH Exp GTGT Cont GTGTGT MCont EVALTO QM { In_EvalTo($1, $5, $7, $3) }
  | Val DRARROW Cont EVALTO QM { In_AppK($3, RetKK, $1) }
  | Val DRARROW Cont GTGTGT MCont EVALTO QM { In_AppK($3, $5, $1) }
  | SInt PLUS SInt IS QM { In_AppBOp(Plus, Value_of_int $1, Value_of_int $3) }
  | SInt MULT SInt IS QM { In_AppBOp(Mult, Value_of_int $1, Value_of_int $3) }
  | SInt MINUS SInt IS QM { In_AppBOp(Minus, Value_of_int $1, Value_of_int $3) }
/*  | SInt IS LESS THAN SInt { In_AppBOp(Lt, Value_of_int $1, Value_of_int $5) }
  | SInt IS NOT LESS THAN SInt { AppBOp(Lt, Value_of_int $1, Value_of_int $6) }
*/
  | Env VDASH Exp error { errAt 4 "Syntax error: 'evalto' or '>>' expected" }
  | Env VDASH Exp EVALTO error { errAt 5 "Syntax error: '?' expected" }
  | Env VDASH Exp GTGT error { errAt 5 "Syntax error: continuation expected" }
  | Env VDASH Exp GTGT Cont error { errAt 6 "Syntax error: 'evalto' expected" }
  | Env VDASH Exp GTGT Cont EVALTO error { errAt 7 "Syntax error: '?' expected" }
  | Val DRARROW error { errAt 3 "Syntax error: continuation expected" }
  | Val DRARROW Cont error { errAt 4 "Syntax error: 'evalto' expected" }
  | Val DRARROW Cont EVALTO error { errAt 5 "Syntax error: '?' expected" }
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
  | MATCH Exp WITH LBRACKET RBRACKET RARROW Exp BAR LCID COLCOL LCID RARROW Exp
      { if $9 = $11
	then errBtw 9 11 "These variables shouldn't be the same"
	else Match($2, $7, Var $9, Var $11, $13) }
  | SHIFT LCID IN Exp { Shift(Var $2, $4) }

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
  | SHIFT error { errAt 2 "Syntax error: variable expected" }
  | SHIFT LCID error { errAt 3 "Syntax error: 'in' expected" }
  | SHIFT LCID IN error { errAt 4 "Syntax error: expression expected" }

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
  | LBRACE Exp RBRACE { Reset $2 }

  | LPAREN error { errAt 2 "Syntax error: expression expected" }
  | LPAREN Exp error { errBtw 1 3 "Syntax error: unmatched parentheses" }
  | LBRACKET error { errAt 2 "Syntax error: ']' expected" }
  | LBRACE error { errAt 2 "Syntax error: expression expected" }
  | LBRACE Exp error { errBtw 1 3 "Syntax error: '}' expected" }


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
  | LBRACKET Cont RBRACKET { ContF $2 }

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

BinOp: BinOp1 {$1} | BinOp3 {$1} | BinOp4 {$1}

MHole: USCOREUSCORE { RetKK }

MCont:
    MHole { RetKK }
  | Cont OptMCont { DelimitedKK($1, $2) }
OptMCont: GTGTGT MCont { $2 }
  | /* empty */ { RetKK }
  | GTGTGT error { errAt 2 "Syntax error: meta-continuation expected" }

Hole:  UNDERSCORE { RetK }

OptCont: GTGT Cont { $2 }
  | /* empty */ { RetK }
  | GTGT error { errAt 2 "Syntax error: continuation expected" }
Cont:
  | Hole { RetK }
  | LBRACE Env VDASH Hole BinOp1 Exp2 RBRACE OptCont
     { EvalRK($2, $6, $5, $8) }
  | LBRACE Env VDASH Hole BinOp3 Exp4 RBRACE OptCont
     { EvalRK($2, $6, $5, $8) }
  | LBRACE Env VDASH Hole BinOp4 Exp5 RBRACE OptCont
     { EvalRK($2, $6, $5, $8) }
  | LBRACE Env VDASH Hole BinOp LongExp RBRACE OptCont
     { EvalRK($2, $6, $5, $8) }
  | LBRACE Val BinOp Hole RBRACE OptCont { AppOpK($2, $3, $6) }
  | LBRACE Env VDASH IF Hole THEN Exp ELSE Exp RBRACE OptCont
     { BranchK($2, $7, $9, $11) }
  | LBRACE Env VDASH LET LCID EQ Hole IN Exp RBRACE OptCont
     { LetBodyK($2, Var $5, $9, $11) }
  | LBRACE Env VDASH Hole AExp RBRACE OptCont { EvalArgK($2, $5, $7) }
  | LBRACE Val Hole RBRACE OptCont { AppFunK($2, $5) }
  | LBRACE Env VDASH Hole COLCOL Exp2 RBRACE OptCont { EvalConsRK($2, $6, $8) }
  | LBRACE AVal COLCOL Hole RBRACE OptCont { ConsK($2, $6) }
  | LBRACE Env VDASH MATCH Hole WITH LBRACKET RBRACKET RARROW Exp
                                 BAR LCID COLCOL LCID RARROW Exp RBRACE OptCont
      { if $12 = $14
	then errBtw 12 14 "These variables shouldn't be the same"
	else MatchK($2, $10, Var $12, Var $14, $16, $18) }

  | LBRACE error { errAt 2 "Syntax error: '_' or value or environment expected" }
  | LBRACE Env VDASH error { errAt 4 "Syntax error: '_', 'if', or 'let' expected" }
  | LBRACE Env VDASH Hole error { errAt 5 "Syntax error: '+', '-', '*', '<', or expression expected" }
  | LBRACE Env VDASH Hole BinOp1 error { errAt 6 "Syntax error: expression expected" }
  | LBRACE Env VDASH Hole BinOp1 Exp2 error { errAt 7 "Syntax error: '}' expected" }
  | LBRACE Env VDASH Hole BinOp3 error { errAt 6 "Syntax error: expression expected" }
  | LBRACE Env VDASH Hole BinOp3 Exp4 error { errAt 7 "Syntax error: '}' expected" }
  | LBRACE Env VDASH Hole BinOp4 error { errAt 6 "Syntax error: expression expected" }
  | LBRACE Env VDASH Hole BinOp4 Exp5 error { errAt 7 "Syntax error: '}' expected" }
  | LBRACE Env VDASH Hole BinOp LongExp error { errAt 7 "Syntax error: '}' expected" }
  | LBRACE Val error { errAt 3 "Syntax error: '+', '-', '*', or '<' expected" }
  | LBRACE Val BinOp error { errAt 4 "Syntax error: '_' expected" }
  | LBRACE Val BinOp Hole error { errAt 5 "Syntax error: '}' expected" }
  | LBRACE Env VDASH IF error { errAt 5 "Syntax error: '_' expected" }
  | LBRACE Env VDASH IF Hole error { errAt 6 "Syntax error: 'then' expected" }
  | LBRACE Env VDASH IF Hole THEN error
     { errAt 7 "Syntax error: expression expected" }
  | LBRACE Env VDASH IF Hole THEN Exp error
     { errAt 8 "Syntax error: 'else' expected" }
  | LBRACE Env VDASH IF Hole THEN Exp ELSE error
     { errAt 9 "Syntax error: expression expected" }
  | LBRACE Env VDASH IF Hole THEN Exp ELSE Exp error
     { errAt 10 "Syntax error: '}' expected" }
  | LBRACE Env VDASH LET error
     { errAt 5 "Syntax error: variable name expected" }
  | LBRACE Env VDASH LET LCID error
     { errAt 6 "Syntax error: '=' expected" }
  | LBRACE Env VDASH LET LCID EQ error
     { errAt 7 "Syntax error: '_' expected" }
  | LBRACE Env VDASH LET LCID EQ Hole error
     { errAt 8 "Syntax error: 'in' expected" }
  | LBRACE Env VDASH LET LCID EQ Hole IN error
     { errAt 9 "Syntax error: expression expected" }
  | LBRACE Env VDASH LET LCID EQ Hole IN Exp error
     { errAt 10 "Syntax error: '}' expected" }
  | LBRACE Env VDASH Hole AExp error { errAt 6 "Syntax error: '}' expected" }
  | LBRACE Val Hole error { errAt 4 "Syntax error: '}' expected" }
  | LBRACE Env VDASH Hole COLCOL error
      { errAt 6 "Syntax error: expression expected" }
  | LBRACE Env VDASH Hole COLCOL Exp2 error
      { errAt 7 "Syntax error: '}' expected" }
  | LBRACE AVal COLCOL Hole error { errAt 5 "Syntax error: '}' expected" }
  | LBRACE Env VDASH MATCH error { errAt 5 "Syntax error: '_' expected" }
  | LBRACE Env VDASH MATCH Hole error
      { errAt 6 "Syntax error: 'with' expected" }
  | LBRACE Env VDASH MATCH Hole WITH error
      { errAt 7 "Syntax error: '[]' expected" }
  | LBRACE Env VDASH MATCH Hole WITH LBRACKET error
      { errAt 8 "Syntax error: '[]' expected" }
  | LBRACE Env VDASH MATCH Hole WITH LBRACKET RBRACKET error
      { errAt 9 "Syntax error: '->' expected" }
  | LBRACE Env VDASH MATCH Hole WITH LBRACKET RBRACKET Exp error
      { errAt 10 "Syntax error: '|' expected" }
  | LBRACE Env VDASH MATCH Hole WITH LBRACKET RBRACKET Exp BAR error
      { errAt 11 "Syntax error: variable name expected" }
  | LBRACE Env VDASH MATCH Hole WITH LBRACKET RBRACKET Exp BAR LCID error
      { errAt 12 "Syntax error: '::' name expected" }
  | LBRACE Env VDASH MATCH Hole WITH LBRACKET RBRACKET Exp
                                BAR LCID COLCOL error
      { errAt 13 "Syntax error: variable name expected" }
  | LBRACE Env VDASH MATCH Hole WITH LBRACKET RBRACKET Exp
                                BAR LCID COLCOL LCID error
      { errAt 14 "Syntax error: '->' expected" }
  | LBRACE Env VDASH MATCH Hole WITH LBRACKET RBRACKET Exp
                                BAR LCID COLCOL LCID RARROW error
      { errAt 15 "Syntax error: expression expected" }
  | LBRACE Env VDASH MATCH Hole WITH LBRACKET RBRACKET Exp
                                BAR LCID COLCOL LCID RARROW Exp error
      { errAt 16 "Syntax error: '}' expected" }

/******** experimental feature for macro defintions *********/

MacroDefs:
  | MacroDef MacroDefs { () }

MacroDef:
  | DEF MVEXP EQ Exp SEMI { Hashtbl.add tbl $2 (Exp $4) }
  | DEF MVVALUE EQ Val SEMI { Hashtbl.add tbl $2 (Value $4) }
  | DEF MVENV EQ Env SEMI { Hashtbl.add tbl $2 (Env $4) }
  | DEF MVCONT EQ Cont SEMI { Hashtbl.add tbl $2 (Cont $4) }
  | DEF MVMCONT EQ MCont SEMI { Hashtbl.add tbl $2 (MCont $4) }

  | DEF MVEXP EQ error { errAt 4 "Syntax error: expression expected" }
  | DEF MVVALUE EQ error { errAt 4 "Syntax error: value expected" }
  | DEF MVENV EQ error { errAt 4 "Syntax error: environment expected" }
  | DEF MVCONT EQ error { errAt 4 "Syntax error: continuation expected"  }
  | DEF MVMCONT EQ error { errAt 4 "Syntax error: meta-continuation expected"  }
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

Cont: MVCONT {
  try
    match Hashtbl.find tbl $1 with
      Cont k -> k
    | _ -> errAt 1 "Cannot happen! Cont: MVCONT"
  with Not_found -> errAt 1 ("Undefined macro: " ^ $1)
  }

MCont: MVMCONT {
  try
    match Hashtbl.find tbl $1 with
      MCont k -> k
    | _ -> errAt 1 "Cannot happen! MCont: MVMCONT"
  with Not_found -> errAt 1 ("Undefined macro: " ^ $1)
  }
