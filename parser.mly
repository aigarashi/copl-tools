%{
open Syntax
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

%start toplevel
%type <Syntax.game> toplevel
%%

toplevel :
     HEADER1 BnfDefs HEADER2 JdgDecls HEADER3 RuleDecls EOF
     { { syndefs = $2; jdgdecls = $4; ruledefs = $6 } }

BnfDefs :
/* empty */ { [] }
  | LCID IN UCID COLCOLEQ BarForms BnfDefs 
     { { mvar = $1; cat = $3; body = $5} :: $6 }
  | LCID IN LCID BnfDefs  /* ML primitive type */
     { { mvar = $1; cat = $3; body = []} :: $4 }

BarForms :
    Form { [ $1 ] }
  | Form BAR BarForms { $1 :: $3 }

Form : 
    LCID { Var $1 }
  | UCID { App($1, []) }
  | UCID LPAREN ComLCIDs RPAREN { App($1, $3) }

ComLCIDs :
    LCID { [ Var $1 ] }
  | LCID COMMA ComLCIDs { Var $1 :: $3 }

JdgDecls :
    JForm SEMI JFormSEMIs { $1 :: $3 }

JForm :
    UCID LPAREN ComLCIDs RPAREN { { pred = $1; args = $3 } }

JFormSEMIs :
    /* empty */ { [] }
  | JForm SEMI JFormSEMIs { $1 :: $3 }

RuleDecls :
    Rule SEMI RuleSEMIs { $1 :: $3 }

Rule :
    Name COLON Judgment COLHYP PremiseList { 
	{ rname = $1; rconc = $3; rprem = $5 }
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

ComTerms :
    Term { [ $1 ] } 
  | Term COMMA ComTerms { $1 :: $3 }

Term : 
    LCID { Var $1 }
  | UCID { App($1, []) }
  | UCID LPAREN ComTerms RPAREN { App($1, $3) }

PremiseList :
    /* empty */ { [] }
  | Judgment { [ J $1 ] }
  | MLexp { [ Qexp $1 ] }
  | Judgment COMMA PremiseList { J $1 :: $3 }
  | MLexp COMMA PremiseList { Qexp $1 :: $3 }
