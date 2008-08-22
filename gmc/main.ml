open Lexing

open Syntax
open Format

type mode = ML | TeX | SExp

let mode = ref ML
let filename = ref ""
let gname = ref ""

let spec = [("-TeX", Arg.Unit (fun () -> mode := TeX), "display rules in TeX");
	    ("-sexp", 
	    Arg.String (fun s -> mode := SExp; gname := s), "display rules for game in sexp")]

let parse_file (s : string (* as file name*)) =
  let lexbuf = Lexing.from_channel (open_in s) in
  let pos = lexbuf.lex_curr_p in
	      lexbuf.lex_curr_p <- { pos with pos_fname = !filename };
  Parser.toplevel Lexer.main lexbuf

let emit_game (g : Syntax.game) = 
  let env = Env.of_game g in
    printf "(* @["; 
    Syntax.Env.print_env env;
    printf "@] *)\n\n";
  print_string "open MySupport.Error"; print_newline ();
  print_string "open MySupport.Pervasives"; print_newline ();
  print_string "open Derivation"; print_newline ();
  Emit.typedef env std_formatter g.syndefs; print_newline ();
  Emit.jdgdef env std_formatter g.jdgdecls; print_newline ();
  print_newline();
  (match g.mldefs with Some s -> print_string s; print_newline() | None -> ());
  Emit.rules env std_formatter g.ruledefs;
  print_newline ();  print_newline ();

  (* experimental prover generation *)
  Emit.Prover.emit_jdgdef env std_formatter g.jdgdecls;
  if Mode.check_rules env g.ruledefs 
  then (* mode checking succeeds *)
    begin
      pp_print_string std_formatter "\

let dummy = Lexing.dummy_pos
let deriv_stack = Stack.create ()

exception NoApplicableRule of in_judgment
";
      Emit.Prover.emit env std_formatter g.ruledefs
    end
  else (* mode check fails *)
      pp_print_string std_formatter "\

exception NoApplicableRule of in_judgment

let make_deriv _ = failwith \"make_deriv not implemented due to mode analysis failure\"
"

let _ = 
  Arg.parse spec (fun s -> filename := s) "Usage: gmc [-TeX | -sexp <game>] filename";
  let g = parse_file !filename in
    match !mode with
	ML -> emit_game g
      | TeX -> Emit.tex_rules g.ruledefs
      | SExp -> Emit.sexp_rules !gname g.ruledefs
