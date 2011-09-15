toplevel: 
    MacroDefs Derivation { $2 }
  | error { errAt 1 "Syntax error, perhaps at the beginning of the input" }
  | EOF { raise End_of_file }  

judgment: Judgment { $1 }

Derivation: 
   Judgment BY RName LBRACE RBRACE
    { {conc = $1; by = $3; since = []; pos = rhs_start_pos 3, rhs_end_pos 3 } }
  | Judgment BY RName LBRACE Derivs
    { {conc = $1; by = $3; since = $5; pos = rhs_start_pos 3, rhs_end_pos 3 } }
  | Judgment QM { {conc = $1; by = None; since = []; pos = rhs_start_pos 2, rhs_end_pos 2} }
  | Judgment error { errAt 2 "Syntax error: 'by' expected after a judgment" }
  | Judgment BY error { errAt 3 "Syntax error: rule name expected after 'by'" }
  | Judgment BY RName error { errAt 4 "Syntax error: opening brace expected" }
  | Judgment BY RName LBRACE error { errBtw 4 5 "Syntax error: unmatched brace" }

RName : 
    ID { Some $1 }
  | LCID { Some $1 }

Derivs:
  | Derivation RBRACE { [ $1 ] }
  | Derivation SEMI RBRACE { [ $1 ] } 
  | Derivation SEMI Derivs { $1::$3 }
  | Derivation error { errAt 2 "Syntax error: unmatched brace, or semicolon forgotten?" }

MacroDefs: { () }
