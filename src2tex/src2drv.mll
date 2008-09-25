(*
 * ML lex script to convert .src files to .pi files.
 *)

{
  type mode = ML | TEX

  let mode = ref TEX

  let current = ref ""
  let emit s = current := (!current ^ s)

  let basename = ref ""
  let wholename = ref ""
  let suffix = ref "drv"
  let count = ref 0
  let filename_of c = !basename ^ "." ^ (string_of_int c) ^ "." ^ !suffix
  let dump () =
    if !mode = ML then
      begin
	count := !count + 1;
	let outname = filename_of !count in
	let o = open_out outname in
	output_string o !current;
	close_out o;
      end;
     current := ""

  let header = 
"#!/bin/bash\n$HOME/work/books/CoPL/tools/checker/checker -full -game "

  let pr s = if !mode = ML then emit s
}

rule lex = parse
  eof { }
| "#{" ['#' '*'] "}{" (['A'-'Z' 'a'-'z' '(' ')' '-' ' ' '"' '?']* as opt) "}\n" {
    pr (Lexing.lexeme lexbuf); mode := ML; 
    prerr_string "hoge!\n";
    emit header;
    emit opt;
    emit " <<EOD\n";
    lex lexbuf  }
| "#{@}" { emit "\nEOD\n";
	   (match !mode with ML -> dump () | _ -> ());
	   mode := TEX;
	   lex lexbuf }
| _ { pr (Lexing.lexeme lexbuf); lex lexbuf  }

{
  let () =
    if Array.length Sys.argv == 3 then
      (wholename := Array.get Sys.argv 1;
       basename := Array.get Sys.argv 2;
(*       emit "(* \""; emit !wholename; emit "\" *)\n"; *)
       lex (Lexing.from_channel (open_in !wholename)))
    else
      (basename := "ml/f"; 
       lex (Lexing.from_channel stdin));
    exit 0
}
