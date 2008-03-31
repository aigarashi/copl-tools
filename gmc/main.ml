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
  print_string "open MySupport.Pervasives"; print_newline ();
  print_string "open Derivation"; print_newline ();
  Emit.typedef env g.syndefs; print_newline ();
  Emit.jdgdef env g.jdgdecls; print_newline ();
  print_newline();
  Emit.rules env g.ruledefs;
  print_newline ();  print_newline ();
  (* experimental prover generation *)
  Emit.Prover.emit_jdgdef env std_formatter g.jdgdecls;
  pp_print_string std_formatter "\

let dummy = Lexing.dummy_pos
let deriv_stack = Stack.create ()

exception NoApplicableRule of in_judgment
";
  Emit.Prover.emit env std_formatter g.ruledefs

let _ = 
  Arg.parse spec (fun s -> filename := s) "Usage: gmc [-TeX] filename";
  let g = parse_file !filename in
    match !mode with
	ML -> emit_game g
      | TeX -> Emit.tex_rules g.ruledefs
