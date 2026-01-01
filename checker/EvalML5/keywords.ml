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

  (* ML1 expressions *)
  ("true", TRUE);
  ("false", FALSE);
  ("if", IF);
  ("then", THEN);
  ("else", ELSE);

  ("*", AST);
  ("+", CROSS);
  ("-", HYPHEN);
  ("<", LT);

  (* ML2 judgments and expressions *)
  ("|-", VDASH);
  (",", COMMA);

  ("let", LET);
  ("in", IN);
  ("=", EQ);

  (* ML3 expressions *)
  ("->", RARROW);
  ("fun", FUN);

  (* ML4 expressions *)
  ("rec", REC);

  (* ML5 expressions *)
  ("match", MATCH);
  ("with", WITH);
  ("|", BAR);
  ("::", COLCOL);

  (* ML6 expressions *)
  ("_", UNDERBAR);
  ("matches", MATCHES);
  ("doesn't", DOESNT);
  ("when", WHEN);
]
