open Parser

let v = [
  (* game-specific keywords *)
  ("evalto", EVALTO);
  
  ("if", IF);
  ("then", THEN);
  ("else", ELSE);
  ("true", TRUE);
  ("false", FALSE);
  ("while", WHILE);
  ("do", DO);
  ("skip", SKIP);
  
  ("|-", VDASH);
  (",", COMMA);

  ("*", AST);
  ("+", CROSS);
  ("-", HYPHEN);
  ("!", BANG);
  ("&&", AMPAMP);
  ("||", BARBAR);
  ("<", LT);
  ("=", EQ);
  ("<=", LE);
  (":=", COLEQ);
] 

