open Syntax
open Format

let parse_file s =
  Parser.toplevel Lexer.main (Lexing.from_channel (open_in s))

let emit_game g = 
  let env = Env.of_game g in
    printf "(* @["; 
    Syntax.Env.print_env env;
    printf "@] *)"; print_newline(); print_newline();
  print_string "open MySupport.Error"; print_newline ();
  print_string "open Syntax"; print_newline ();
  Emit.typedef env g.syndefs; print_newline ();
  Emit.jdgdef env g.jdgdecls; print_newline ();
  print_newline();
  Emit.rules env g.ruledefs;
  print_newline ()

let _ = emit_game (parse_file Sys.argv.(1))

  
  
