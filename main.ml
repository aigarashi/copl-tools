open Syntax
open Format

let parse_file s =
  Parser.toplevel Lexer.main (Lexing.from_channel (open_in s))

let emit_game g = 
  let env = Env.of_game g in
  Pp.typedef env g.syndefs;
  print_newline ();
  Pp.jdgdef env g.jdgdecls;
  print_newline ();
print_string 
"open MySupport.Error

type rulename = string

type derivation = {
  conc:  judgment;
  by:    rulename;
  since: derivation list;
  pos:   pos
}
";
(*
type rule = {
  name : rulename;
  conc : judgment;
  premises : judgment;
}  

*)
  print_newline();
  Pp.rules env g.ruledefs;
  print_newline ()

let _ = emit_game (parse_file Sys.argv.(1))

  
  
