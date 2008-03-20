open Lexing

let parse_file s =
  let lexbuf = Lexing.from_channel (open_in s) in
  let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with pos_fname = s };
    Parser.toplevel Lexer.main lexbuf

let d = parse_file Sys.argv.(1)

let _ = 
  let j = Core.deriv_check d in
    Syntax.print_deriv Pp.print_judgment Format.std_formatter d;
    Format.print_newline()


  
  
