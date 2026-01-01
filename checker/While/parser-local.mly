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
  | Comm CHANGES St TO St { Exec($3, $1, $5) }

  | St VDASH AExp error { errAt 4 "Syntax error: 'evalto' expected" }
  | St VDASH AExp EVALTO error { errAt 5 "Syntax error: integer expected" }
  | St VDASH BExp error { errAt 4 "Syntax error: 'evalto' expected" }
  | St VDASH BExp EVALTO error { errAt 5 "Syntax error: true or false expected" }

partialj :
    St VDASH AExp EVALTO QM { In_AEvalTo($1, $3) }
  | St VDASH BExp EVALTO QM { In_BEvalTo($1, $3) }
  | Comm CHANGES St TO QM { In_Exec($3, $1) }

St:
    /* empty */ { Empty }
  | LCID EQ SInt St2 { List.fold_left (fun st (id, v) -> Bind(st, Var id, v)) Empty (($1,$3)::$4) }
  | LCID error { errAt 2 "Syntax error: '=' expected" }
  | LCID EQ error { errAt 3 "Syntax error: integer expected" }

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
    BExp3 { $1 }
  | BANG BExp4 { Not $2 }

BExp3:
    TRUE { BExp_of_bool true }
  | FALSE { BExp_of_bool false }
  | AExp LT AExp { COp(Lt, $1, $3) }
  | AExp EQ AExp { COp(Eq, $1, $3) }
  | AExp LE AExp { COp(Le, $1, $3) }
  | LPAREN BExp RPAREN { $2 }

BExp4:
    TRUE { BExp_of_bool true }
  | FALSE { BExp_of_bool false }
  | BANG BExp4 { Not $2 }
  | LPAREN BExp RPAREN { $2 }

Comm:
    Comm1 { $1 }
  | IF BExp THEN Comm ELSE Comm { If($2, $4, $6) }
  | WHILE LPAREN BExp RPAREN DO Comm { While($3, $6) }

Comm1:
    Comm2 { $1 }
  | Comm2 SEMI Comm1 { Seq($1, $3) }
  | Comm2 SEMI IF BExp THEN Comm ELSE Comm { Seq($1, If($4, $6, $8)) }
  | Comm2 SEMI WHILE LPAREN BExp RPAREN DO Comm { Seq($1, While($5, $8)) }

Comm2:
    SKIP { Skip }
  | LCID COLEQ AExp { Assign(Var $1, $3) }
  | LPAREN Comm RPAREN { $2 }
