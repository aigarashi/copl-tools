open Lexing

let parse_file s =
  let lexbuf = Lexing.from_channel (open_in s) in
  let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with pos_fname = s };
    Parser.toplevel Lexer.main lexbuf

let _ = Core.deriv_check (parse_file Sys.argv.(1))


  
  
