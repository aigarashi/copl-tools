open Parser

let v = [
  (* common keywords and symbols *)
  ("by", BY);
  (";", SEMI);
  ("?", QM);

  (* game-specific keywords *)
  ("S", S);
  ("Z", Z);
  ("evalto", EVALTO);
  ("plus", PLUS);
  ("mult", MULT);
  ("is", IS);
  ("*", AST);
  ("+", CROSS);
] 
