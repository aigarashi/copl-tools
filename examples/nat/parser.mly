%{
open Core
%}

%token EOF

%token BY
%token LBRACE RBRACE LPAREN RPAREN
%token SEMI
%token <Core.rulename> ID

%token PLUS EVALTO MULT IS
%token AST CROSS S Z

%start toplevel
%type <Core.derivation> toplevel

/*
  judgment by rule {
    subderiv_1;
    ...
    subderiv_n
  }

<<Judgments>>
  Exp evalto Nat
  Nat plus Nat is Nat
  Nat mult Nat is Nat

*/

%%

toplevel: 
    Derivation { $1 }

Derivation: 
    Judgment BY ID LBRACE RBRACE
    { {conc = $1; by = $3; since = []; pos = rhs_start_pos 3 } }
  | Judgment BY ID LBRACE Derivs
    { {conc = $1; by = $3; since = $5; pos = rhs_start_pos 3 } }
  | Judgment error { failwith "Syntax error: \"by\" expected" }
  | Judgment BY ID error { failwith "Syntax error: opening brace expected" }
  | Judgment BY ID LBRACE error { failwith "Syntax error: unmatched brace" }

Derivs:
  | Derivation RBRACE { [ $1 ] }
  | Derivation SEMI RBRACE { [ $1 ] } 
  | Derivation SEMI Derivs { $1::$3 }
  | Derivation error { failwith "Syntax error: unmatched brace, or semicolon forgotten?" }

Judgment: 
    Exp EVALTO Nat { EvalTo($1, $3) }
  | Nat PLUS Nat IS Nat { PlusIs($1, $3, $5) }
  | Nat MULT Nat IS Nat { MultIs($1, $3, $5) }

  | Exp EVALTO error { failwith "Syntax error: natural number expected" }
  | Nat PLUS error { failwith "Syntax error: natural number expected" }
  | Nat PLUS Nat error { failwith "Syntax error: \'is\' expected" }
  | Nat PLUS Nat IS error { failwith "Syntax error: natural number expected" }
  | Nat MULT error { failwith "Syntax error: natural number expected" }
  | Nat MULT Nat error { failwith "Syntax error: \'is\' expected" }
  | Nat MULT Nat IS error { failwith "Syntax error: natural number expected" }

Exp:
    Exp CROSS MExp { P($1, $3) }
  | MExp { $1 }

MExp:
    MExp AST AExp { M($1, $3) }
  | AExp { $1 }

AExp:
    Nat { Exp_of_Nat $1 }
  | LPAREN Exp RPAREN { $2 }
  | LPAREN Exp error { failwith "Syntax error: unmatched parenthesis" }

Nat:
    Z { Z }
  | S LPAREN Nat RPAREN { S $3 }
  | S LPAREN Nat error { failwith "Syntax error: unmatched parenthesis" }
  | S error { failwith "Syntax error: opening parenthesis expected after S" }

