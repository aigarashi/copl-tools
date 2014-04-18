%{
open Core
open Derivation

let errBtw i j s =
  MySupport.Error.errBtw 
    (Parsing.rhs_start_pos i) (Parsing.rhs_end_pos j) s

let errAt i s =
  MySupport.Error.errBtw (Parsing.rhs_start_pos i) (Parsing.rhs_end_pos i) s

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

%token EVALTO CHANGES TO
%token AST CROSS HYPHEN BANG AMPAMP BARBAR LT EQ LE COLEQ

%token IF THEN ELSE TRUE FALSE WHILE DO SKIP

%token VDASH COMMA

%start toplevel partialj judgment
%type <Core.judgment Derivation.t> toplevel
%type <Core.judgment> judgment


%token QM /* stands for question mark to specify holes in a judgment */
%type <Core.in_judgment> partialj

/******** experimental feature for macro defitinions *********/
%token DEF EQ
%token <string> MVEXP
%token <string> MVVALUE
%token <string> MVENV
%%

Judgment: 
    St VDASH AExp EVALTO SInt { AEvalTo($1, $3, $5) }
  | St VDASH BExp EVALTO TRUE { BEvalTo($1, $3, true) }
  | St VDASH BExp EVALTO FALSE { BEvalTo($1, $3, false) }
  | Com CHANGES St TO St { Exec($3, $1, $5) }

  | St VDASH AExp error { errAt 4 "Syntax error: 'evalto' expected" }
  | St VDASH AExp EVALTO error { errAt 5 "Syntax error: integer expected" }
  | St VDASH BExp error { errAt 4 "Syntax error: 'evalto' expected" }
  | St VDASH BExp EVALTO error { errAt 5 "Syntax error: true or false expected" }

partialj :
    St VDASH AExp EVALTO QM { In_AEvalTo($1, $3) }
  | St VDASH BExp EVALTO QM { In_BEvalTo($1, $3) }

St:
    /* empty */ { Empty } 
  | LCID EQ SInt St2 { List.fold_left (fun st (id, v) -> Bind(st, Var id, v)) Empty (($1,$3)::$4) }
  | LCID error { errAt 2 "Syntax error: '=' expected" }
  | LCID EQ error { errAt 3 "Syntax error: value expected" }

St2:
    /* empty */ { [] }
  | COMMA LCID EQ SInt St2 { ($2, $4) :: $5 }
  | error { errAt 1 "Syntax error: comma expected" }
  | COMMA error { errAt 2 "Syntax error: variable expected" }
  | COMMA LCID error { errAt 3 "Syntax error: '=' expected" }
  | COMMA LCID EQ error { errAt 4 "Syntax error: value expected" }
  
AExp:
  | AExp1 { $1 }
  | AExp CROSS AExp1 { AOp(Plus, $1, $3) }
  | AExp HYPHEN AExp1 { AOp(Minus, $1, $3) }

AExp1:
  | AExp2 { $1 }
  | AExp1 AST AExp2 { AOp(Mult, $1, $3) }

AExp2:
    SInt { AExp_of_int $1 }
  | LCID { AExp_of_Var (Var $1) }
  | LPAREN AExp RPAREN { $2 }

  | LPAREN error { errAt 2 "Syntax error: expression expected" }
  | LPAREN AExp error { errBtw 1 3 "Syntax error: unmatched parenthesis" }

SInt: /* signed int */
    INTL { $1 }
  | HYPHEN INTL { - $2 }

BExp:
    BExp1 { $1 }
  | BExp1 BARBAR BExp2 { LOp(Or, $1, $3) }

BExp1:
    BExp2 { $1 }
  | BExp2 AMPAMP BExp1 { LOp(And, $1, $3) }

BExp2:
    TRUE { BExp_of_bool true }
  | FALSE { BExp_of_bool false }
  | AExp LT AExp { COp(Lt, $1, $3) }
  | AExp EQ AExp { COp(Eq, $1, $3) }
  | AExp LE AExp { COp(Le, $1, $3) }
  | BANG BExp2 { Not $2 }
  | LPAREN BExp RPAREN { $2 }

Com:
    Com1 { $1 }
  | IF BExp THEN Com1 ELSE Com1 { If($2, $4, $6) }
  | WHILE LPAREN BExp RPAREN DO Com1 { While($3, $6) }

Com1:
    Com2 { $1 }
  | Com2 SEMI Com1 { Seq($1, $3) }

Com2:
    SKIP { Skip }
  | LCID COLEQ AExp { Assign(Var $1, $3) }
  | LPAREN Com RPAREN { $2 }
