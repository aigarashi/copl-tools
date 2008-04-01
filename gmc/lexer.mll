{
open Lexing

let reservedWords = [
  (* Keywords *)
  ("in", Parser.IN);
] 

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

| "|" { Parser.BAR }
| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| "::=" { Parser.COLCOLEQ }
(*| ":=" { Parser.COLEQ } *)
| ":-" { Parser.COLHYP }
| ":" { Parser.COLON }
| "," { Parser.COMMA }
| ";" { Parser.SEMI }
| "[BNF]" { Parser.HEADER1 }
| "[Judgments]" { Parser.HEADER2 }
| "[Rules]" { Parser.HEADER3 }

| ['a'-'z']+ ['0'-'9']*
    { let id = Lexing.lexeme lexbuf in
      try 
        List.assoc id reservedWords
      with
      _ -> Parser.LCID id
     }
| ['A'-'Z'] ['a'-'z' 'A'-'Z']* ['0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try 
        List.assoc id reservedWords
      with
      _ -> Parser.UCID id
     }
| ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'' '-']* {
      Parser.SYMID (Lexing.lexeme lexbuf)
}
| '\"' [^ '\"' '\n' '\t' ' ']* '\"' { 
      let name = Lexing.lexeme lexbuf in
	Parser.SYMID (String.sub name 1 (String.length name - 2))
    }
| '`' [^ '`' '\n']* '`' {
      let name = Lexing.lexeme lexbuf in
	Parser.MLexp (String.sub name 1 (String.length name - 2))
    }
| "(*" { comment 1 lexbuf }
| eof { Parser.EOF }

and comment n = parse
  "(*" { comment (n+1) lexbuf }
| "*)" { if n = 1 then main lexbuf else comment (n-1) lexbuf }
| [^ '\n'] { comment n lexbuf }
| '\n' { newline lexbuf; comment n lexbuf }
