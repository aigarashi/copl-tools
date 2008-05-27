%{
open Syntax

let errBtw i j s =
  MySupport.Error.errBtw 
    (Parsing.rhs_start_pos i) (Parsing.rhs_end_pos j) s

let errAt i s =
  MySupport.Error.errAt (Parsing.rhs_start_pos i) s

let appVar vars = List.map (fun x -> Var x) vars
%}

%token EOF

%token BAR
%token COLCOLEQ COLON COLHYP COMMA
%token HEADER1 HEADER2 HEADER3
%token IN
%token LPAREN RPAREN
%token SEMI
%token <Syntax.id> LCID
%token <Syntax.id> UCID
%token <Syntax.id> SYMID
%token <string> MLexp
%token <string> MLlongexp

%start toplevel
%type <Syntax.game> toplevel
%%

toplevel :
     HEADER1 BnfDefs HEADER2 JdgDecls HEADER3 RuleDecls EOF
     { { syndefs = $2; jdgdecls = $4; ruledefs = $6; mldefs = None } }
   | HEADER1 BnfDefs HEADER2 JdgDecls HEADER3 RuleDecls MLlongexp EOF
     { { syndefs = $2; jdgdecls = $4; ruledefs = $6; mldefs = Some $7 } }

BnfDefs :
/* empty */ { [] }
  | ComLCIDs IN UCID COLCOLEQ BarForms BnfDefs 
     { { mvars = $1; cat = $3; body = $5} :: $6 }
  | ComLCIDs IN LCID BnfDefs  /* ML primitive type */
     { { mvars = $1; cat = $3; body = []} :: $4 }

BarForms :
    Form { [ $1 ] }
  | Form BAR BarForms { $1 :: $3 }

Form : 
    LCID { Var $1 }
  | UCID { App($1, []) }
  | UCID LPAREN ComLCIDs RPAREN { App($1, appVar $3) }
  | UCID LPAREN ComLCIDs error { errBtw 2 4 "Syntax error: unmatched parenthesis" }

ComLCIDs :
    LCID { [ $1 ] }
  | LCID COMMA ComLCIDs { $1 :: $3 }
/*  | LCID error { errAt 2 "Syntax error: comma expected" } */
  | error { errAt 1 "Syntax error: metavariable expected" }

JdgDecls :
    JForm SEMI JFormSEMIs { $1 :: $3 }

JForm :
    UCID LPAREN ComLCIDs RPAREN {
      ({ pred = $1; args = appVar $3 }, List.length $3)
    }
  | UCID LPAREN ComLCIDs SEMI ComLCIDs RPAREN {
      ({ pred = $1; args = appVar ($3 @ $5)}, List.length $3)
    }

JFormSEMIs :
    /* empty */ { [] }
  | JForm SEMI JFormSEMIs { $1 :: $3 }

RuleDecls :
    Rule SEMI RuleSEMIs { $1 :: $3 }
  | Rule error { errAt 2 "Syntax error: semicolon expected" }

Rule :
    Name COLON Judgment COLHYP PremiseList { 
	{ rname = $1; rconc = $3; rprem = $5 }
    } 
  | Name COLON Judgment error { 
	errAt 4 "Syntax error: \":-\" expected"
    } 
  | Name error { 
	errAt 2 "Syntax error: colon expected after a rule name"
    } 

Name : 
    LCID { $1 }
  | UCID { $1 }
  | SYMID { $1 }

RuleSEMIs :
    /* empty */ { [] }
  | Rule SEMI RuleSEMIs { $1 :: $3 }

Judgment : 
    UCID LPAREN ComTerms RPAREN { {pred = $1; args = $3} }
  | UCID LPAREN error { errAt 3 "Syntax error: closing parenthesis expected" }

ComTerms :
    Term { [ $1 ] } 
  | Term COMMA ComTerms { $1 :: $3 }

Term : 
    LCID { Var $1 }
  | UCID { App($1, []) }
  | UCID LPAREN ComTerms RPAREN { App($1, $3) }
  | UCID LPAREN error { errAt 3 "Syntax error: closing parenthesis expected" }

PremiseList :
    /* empty */ { [] }
  | Judgment { [ J $1 ] }
  | MLexp { [ Qexp $1 ] }
  | Judgment COMMA PremiseList { J $1 :: $3 }
  | MLexp COMMA PremiseList { Qexp $1 :: $3 }
