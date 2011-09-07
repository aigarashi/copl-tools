open Lexing

open Syntax
open Format

type mode = ML | TeX | SExp

let mode = ref ML
let filename = ref ""
let gname = ref ""

let spec = [("-TeX", Arg.String (fun s -> mode := TeX; gname := s), "display rules in TeX");
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
  pp_print_string std_formatter "
open MySupport.Error
open MySupport.Pervasives
open Derivation

let failed = ref false
let warning s = warning s; failed := true
let warningAt p s = warningAt p s; failed := true
let warningBtw p1 p2 s = warningBtw p1 p2 s; failed := true

";
  (* Macro variable name list *)
  (*
  Emit.MacroVars.of_bnf env std_formatter g.syndefs;
  *)
  (* Type definition for syntactic entities *)
  Emit.ML.of_bnf env std_formatter g.syndefs;
  pp_print_newline std_formatter ();
  (* Type definition for judgments *)
  Emit.ML.of_judgments env std_formatter g.jdgdecls; 
  pp_print_newline std_formatter ();
  (* Type definition for partial judgments *)
  Emit.Prover.of_judgments env std_formatter g.jdgdecls;
  pp_print_string std_formatter "

exception NoApplicableRule of in_judgment
";
  (* Emit user-supplied ML definitions *)
  (match g.mldefs with 
       Some s -> 
	 pp_print_string std_formatter s;
	 pp_print_newline std_formatter () 
     | None -> ());
  (* Emit the derivation checker *)
  Emit.ML.of_rules env std_formatter g.ruledefs;
  pp_print_newline std_formatter ();  pp_print_newline std_formatter ();

  if Mode.check_rules env g.ruledefs 
  then (* if mode checking succeeds *)
    begin
      (* then, emit the derivation generator *)
      pp_print_string std_formatter "
let dummy = Lexing.dummy_pos
let deriv_stack = Stack.create ()
";
      Emit.Prover.of_rules env std_formatter g.ruledefs
    end
  else (* mode check fails *)
      pp_print_string std_formatter "
let make_deriv _ = failwith \"make_deriv not implemented due to mode analysis failure\"
"

let _ = 
  Arg.parse spec (fun s -> filename := s) 
    "Usage: gmc [-TeX <game> | -sexp <game>] filename";
  let g = parse_file !filename in
    match !mode with
	ML -> emit_game g
      | TeX ->
	  Emit.TeX.of_bnf std_formatter !gname g.syndefs;
	  Emit.TeX.of_rules std_formatter !gname g.ruledefs
      | SExp -> 
	  Emit.SExp.of_bnf std_formatter !gname g.syndefs;
	  Emit.SExp.of_rules std_formatter !gname g.ruledefs
