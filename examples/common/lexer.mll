{
module Make(X : 
  sig
    module Core : sig type judgment end

    module P : 
    sig
      type token
	  
      val toplevel : 
	(Lexing.lexbuf -> token) -> Lexing.lexbuf -> Core.judgment Derivation.t

      val lparen : token
      val rparen : token
      val lbrace : token
      val rbrace : token
      val semi : token
      val intl : int -> token
      val eof : token
      val id : string -> token
    end

    module K : sig val v : (string * P.token) list end
  end
) =
struct 

open X.P

open Lexing

let tbl = 
  let tbl = Hashtbl.create 1024 in
    List.iter (fun (word, token) -> Hashtbl.add tbl word token) X.K.v;
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
| "(" { lparen }
| ")" { rparen }
| "{" { lbrace }
| "}" { rbrace }
| ";" { semi }
| "(*" { comment 1 lexbuf }

(* alphabetical names *)
| ['A'-'Z' 'a'-'z']+ ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'' '-']*
    { let name = Lexing.lexeme lexbuf in
      try 
        Hashtbl.find tbl name
      with
      _ -> id name
     }

(* quoted names *)
| '\"' [^ '\"' '\n' '\t' ' ']* '\"' { 
      let name = Lexing.lexeme lexbuf in
	id (String.sub name 1 (String.length name - 2))
    }

| ['!' '"' '#' '$' '%' '&' '\'' '*' '+' ',' '-' '.' '/' ':' '<' '=' '>' '+' 
   '@' '^' '_' '`' '~' '|' ]+ {
    let sym = Lexing.lexeme lexbuf in
      try Hashtbl.find tbl sym with _ -> id sym
    }

| '-'? ['0' - '9']+
    { intl (int_of_string (Lexing.lexeme lexbuf)) }

| eof { eof }

and comment n = parse
  "(*" { comment (n+1) lexbuf }
| "*)" { if n = 1 then main lexbuf else comment (n-1) lexbuf }
| [^ '\n'] { comment n lexbuf }
| '\n' { newline lexbuf; comment n lexbuf }

{
  end
}
