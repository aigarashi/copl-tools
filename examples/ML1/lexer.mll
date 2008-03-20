{
open Parser
open Lexing

let reservedWords = [
  (* common keywords *)
  ("by", BY);

  (* game-specific keywords *)
  ("evalto", EVALTO);

  ("minus", MINUS);
  ("mult", MULT);
  ("plus", PLUS);
  ("is", IS);

  ("true", TRUE);
  ("false", FALSE);
  ("if", IF);
  ("then", THEN);
  ("else", ELSE);

  ("*", AST);
  ("+", CROSS);
  ("-", HYPHEN);
] 

let tbl = 
  let tbl = Hashtbl.create 1024 in
    List.iter (fun (word, token) -> Hashtbl.add tbl word token) reservedWords;
    tbl

let newline lexbuf =
  let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012']+     { main lexbuf }
  (* ignore spacing and newline characters *)
  | [' ' '\009' '\012']* '\n'    { newline lexbuf; main lexbuf }
  (* ignore # and the following characters until the end of the line *)
  | '#' [^ '\n'] '\n' { newline lexbuf; main lexbuf } 

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
        Hashtbl.find tbl id
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
      try Hashtbl.find tbl sym with _ -> ID sym
    }

| ['0' - '9']+
    { INTL (int_of_string (Lexing.lexeme lexbuf)) }

| eof { EOF }

and comment n = parse
  "(*" { comment (n+1) lexbuf }
| "*)" { if n = 1 then main lexbuf else comment (n-1) lexbuf }
| _ { comment n lexbuf }
