{
open Parser

let reservedWords = [
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

}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }
  (* ignore # and the following characters until the end of the line *)
  | '#' [^ '\n'] '\n' { main lexbuf } 

(* special symbols *)
| "(" { LPAREN }
| ")" { RPAREN }
| "{" { LBRACE }
| "}" { RBRACE }
| ";" { SEMI }
| "(*" { comment 1 lexbuf }

(* alphabetical names *)
| ['A'-'Z' 'a'-'z']+ ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'' '-']*
    { let id = Lexing.lexeme lexbuf in
      try 
        List.assoc id reservedWords
      with
      _ -> ID id
     }

(* quoted names *)
| '\"' [^ '\"' '\n' '\t' ' ']* '\"' { 
      let name = Lexing.lexeme lexbuf in
	ID (String.sub name 1 (String.length name - 2))
    }

| ['!' '"' '#' '$' '%' '&' '\'' '*' '+' ',' '-' '.' '/' ':' '<' '=' '>' '+' 
   '@' '^' '_' '`' '~' '|' ]+ {
    let sym = Lexing.lexeme lexbuf in
      try List.assoc sym reservedWords with _ -> ID sym
    }

| eof { EOF }

and comment n = parse
  "(*" { comment (n+1) lexbuf }
| "*)" { if n = 1 then main lexbuf else comment (n-1) lexbuf }
| _ { comment n lexbuf }
