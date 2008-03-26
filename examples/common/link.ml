open Lexing

module Lexer = Lexer.Make(
    struct
      module Core = Core

      module P = struct
	include Parser

	(* exporting common tokens *)
	open Parser
	let id x = ID x
	and eof = EOF
	and intl i = INTL i
	and semi = SEMI
	and rbrace = RBRACE
	and lbrace = LBRACE
	and rparen = RPAREN
	and lparen = LPAREN
      end 

      module K = Keywords
    end
  )

(*
let filename = ref ""

let fullp = ref false  (* controls whether full derivations are shown *)
let texp = ref false   (* controls whether output is in TeX *)

let spec = [("-full", Arg.Set fullp, "Display full derivations");
	    ("-TeX", Arg.Set texp, "output in TeX")]
*)

let process_input lexbuf fullp texp =
  let d = Parser.toplevel Lexer.main lexbuf in
  let j = Core.deriv_check d in
    (match fullp, texp with
	true, true -> 
	  Derivation.tex_deriv Pp.tex_judgment Format.std_formatter d
      | true, false -> 
	  Derivation.print_deriv Pp.print_judgment Format.std_formatter d
      | false, true ->
	  Pp.tex_judgment Format.std_formatter d.Derivation.conc
      | false, false ->
	  Pp.print_judgment Format.std_formatter d.Derivation.conc);
    Format.print_newline()

(*
let () = 
  Arg.parse spec (fun s -> filename := s) "Usage: [-full] filename";

  let lexbuf = Lexing.from_channel (open_in !filename) in
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with pos_fname = !filename };

  while true do
    process_input lexbuf
  done
*)
