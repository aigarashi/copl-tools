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
	and qm = QM
      end 

      module K = Keywords
    end
  )

let check_deriv lexbuf fullp texp =
  let d = Parser.toplevel Lexer.main lexbuf in
  let j = Core.check_deriv d in
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

let make_deriv lexbuf fullp texp =
  let j = Parser.partialj Lexer.main lexbuf in
  let d = Core.make_deriv j in
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
