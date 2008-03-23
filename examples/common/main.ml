open Lexing

let filename = ref ""

let fullp = ref false  (* controls whether full derivations are shown *)
let texp = ref false   (* controls whether output is in TeX *)

let spec = [("-full", Arg.Set fullp, "Display full derivations");
	    ("-TeX", Arg.Set texp, "output in TeX")]

let () = 
  Arg.parse spec (fun s -> filename := s) "Usage: [-full] filename";

  let lexbuf = Lexing.from_channel (open_in !filename) in
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with pos_fname = !filename };

  while true do
    let d = Parser.toplevel Lexer.main lexbuf in
    let j = Core.deriv_check d in
      (match !fullp, !texp with
	  true, true -> 
	    Syntax.tex_deriv Pp.tex_judgment Format.std_formatter d
	| true, false -> 
	    Syntax.print_deriv Pp.print_judgment Format.std_formatter d
	| false, true ->
	    Pp.tex_judgment Format.std_formatter d.Syntax.conc
	| false, false ->
	    Pp.print_judgment Format.std_formatter d.Syntax.conc);
      Format.print_newline()
  done


  
  
