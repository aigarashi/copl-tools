open Parser

let v = [
  (* game-specific keywords *)
  ("S", S);
  ("Z", Z);
  ("evalto", EVALTO);
  ("plus", PLUS);
  ("times", MULT);
  ("is", IS);
  ("*", AST);
  ("+", CROSS);
]
