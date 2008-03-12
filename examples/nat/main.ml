let parse_file s =
  Parser.toplevel Lexer.main (Lexing.from_channel (open_in s))

let _ = Core.deriv_check (parse_file Sys.argv.(1))


  
  
