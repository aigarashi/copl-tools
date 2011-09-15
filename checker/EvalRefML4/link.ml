open Lexing
open MySupport.Error

module Lexer = Lexer.Make(
    struct
      module Core = Core

      module P = struct
	include Parser

	(* exporting common tokens *)
	open Parser
	let id x = ID x
	and lcid x = LCID x
	and ucid x = UCID x
	and eof = EOF
	and intl i = INTL i
	and semi = SEMI
	and rbrace = RBRACE
	and lbrace = LBRACE
	and rparen = RPAREN
	and lparen = LPAREN
	and rbracket = RBRACKET
	and lbracket = LBRACKET
	and eq = EQ
        and def = DEF
	and qm = QM
        and by = BY
      end 

      module K = Keywords
      module MV = MacroVars
    end
  )

let check_deriv lexbuf ?against fullp texp =
  let d = Parser.toplevel Lexer.main lexbuf in
  let j = Core.check_deriv d in
    if !Core.failed then exit 2
    else
    (match fullp, texp with
	true, true -> 
	  Derivation.tex_deriv Pp.tex_judgment Format.std_formatter d
      | true, false -> 
	  Derivation.print_deriv Pp.print_judgment Format.std_formatter d
      | false, true ->
	  Pp.tex_judgment Format.std_formatter d.Derivation.conc
      | false, false ->
	  Pp.print_judgment Format.std_formatter d.Derivation.conc);
    Format.print_newline();
    match against with 
	None -> () 
      | Some s ->
	  Format.fprintf Format.std_formatter "against %s" s;
	  let j' = Parser.judgment Lexer.main (Lexing.from_string s) in
	    if j <> j' then 
	      err ("The conclusion of the whole judgment is wrong.\nIt should be " ^ s)

let make_deriv lexbuf fullp texp =
  let j = Parser.partialj Lexer.main lexbuf in
    try
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
    with Core.NoApplicableRule j -> 
      Format.fprintf Format.std_formatter 
	"Error: no applicable rule for %a\n" Pp.print_pjudgment j

