open Parser

let v = [
  (* common keywords *)
  ("by", BY);

  (* game-specific keywords *)
  ("evalto", EVALTO);

  ("minus", MINUS);
  ("mult", MULT);
  ("plus", PLUS);
  ("is", IS);
  ("less", LESS);
  ("than", THAN);
  ("not", NOT);

  ("true", TRUE);
  ("false", FALSE);
  ("if", IF);
  ("then", THEN);
  ("else", ELSE);

  ("*", AST);
  ("+", CROSS);
  ("-", HYPHEN);
  ("<", LT);
] 

