open Parser

let v = [
  (* common keywords and symbols *)
  ("by", BY);
  (";", SEMI);
  ("?", QM);

  (* game-specific keywords *)
  (":", COLON);

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

  (* TypingML2 types *)
  ("int", INT);
  ("bool", BOOL);

  (* ML3 expressions *)
  ("->", RARROW);
  ("fun", FUN);

  (* ML4 expressions *)
  ("rec", REC);

  (* TypingML4 *)
  ("'", PRIME);

  (* PolyML4 *)
  ("All", ALL);
] 

