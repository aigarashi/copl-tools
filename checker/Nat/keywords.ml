open Parser

let v = [
  (* common keywords and symbols *)
  ("by", BY);
  (";", SEMI);
  ("?", QM);

  (* game-specific keywords *)
  ("S", S);
  ("Z", Z);
  ("plus", PLUS);
  ("times", MULT);
  ("is", IS);
] 
