open Parser

let v = [
  (* common keywords *)
  ("by", BY);

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
