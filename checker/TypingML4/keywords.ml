open Parser

let v = [
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

  (* ML5 expressions *)
  ("match", MATCH);
  ("with", WITH);
  ("|", BAR);
  ("::", COLCOL);

  (* TypingML5 types *)
  ("list", LIST);
] 
