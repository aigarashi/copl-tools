open Parser

let v = [
  (* common keywords and symbols *)
  ("by", BY);
  (";", SEMI);
  ("?", QM);

  (* game-specific keywords *)
  ("S", S);
  ("Z", Z);
  ("--->", REDUCETO);
  ("-*->", MREDUCETO);
  ("-d->", DREDUCETO);
  ("plus", PLUS);
  ("times", MULT);
  ("is", IS);
  ("*", AST);
  ("+", CROSS);
] 
