open Parser

let v = [
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
