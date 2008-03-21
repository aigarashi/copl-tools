open Lexing

let filename = ref ""

let mode = ref false  (* controls whether full derivations are shown *)

let spec = [("-full", Arg.Set mode, "Display full derivations");]

let () = 
  Arg.parse spec (fun s -> filename := s) "Usage: [-full] filename";

  let lexbuf = Lexing.from_channel (open_in !filename) in
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with pos_fname = !filename };

  while true do
    let d = Parser.toplevel Lexer.main lexbuf in
    let j = Core.deriv_check d in
      if !mode then
	Syntax.print_deriv Pp.print_judgment Format.std_formatter d
      else
	Pp.print_judgment Format.std_formatter d.Syntax.conc;
      Format.print_newline();
  done


  
  
