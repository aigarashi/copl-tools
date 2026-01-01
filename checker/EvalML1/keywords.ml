open Parser

let v = [
  (* game-specific keywords *)
  ("evalto", EVALTO);

  ("minus", MINUS);
  ("times", MULT);
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
