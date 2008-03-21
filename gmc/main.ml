open Syntax
open Format

type mode = ML | TeX

let mode = ref ML
let filename = ref ""

let spec = [("-TeX", Arg.Unit (fun () -> mode := TeX), "display rules in TeX")]

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

let _ = 
  Arg.parse spec (fun s -> filename := s) "Usage: gmc [-TeX] filename";
  let g = parse_file !filename in
    match !mode with
	ML -> emit_game g
      | TeX -> Emit.tex_rules g.ruledefs


  
  
