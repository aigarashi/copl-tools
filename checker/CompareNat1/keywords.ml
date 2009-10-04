open Parser

let v = [
  (* common keywords and symbols *)
  ("by", BY);
  (";", SEMI);
  ("?", QM);

  (* game-specific keywords *)
  ("S", S);
  ("Z", Z);
  ("is", IS);
  ("less", LESS);
  ("than", THAN);
] 
