(*
 * ML lex script to convert .src files to .pi files.
 *)

{
  type mode = ML | TEX | MLsilent

  let mode = ref TEX

  let current = ref ""
  let emit s = current := (!current ^ s)

  let basename = ref ""
  let wholename = ref ""
  let suffix = ref "ml"
  let count = ref 0
  let filename_of c = !basename ^ "." ^ (string_of_int c) ^ "." ^ !suffix
  let dumped_strings = ref ([] : string list)
  let dump newline =
    if !mode = ML then
      begin
	count := !count + 1;
	let outname = filename_of !count in
	let o = open_out outname in
(*     if !count > 1 then 
       output_string o ("#use \"" ^ (filename_of (!count-1)) ^ "\";;\n");
*)
	List.iter (output_string o) !dumped_strings;
	output_string o ("print_string \"Beginning of " ^ outname ^ "\";;");
	output_string o !current; 
	if newline then output_string o "\n";
	close_out o;
      end;
     dumped_strings := !dumped_strings @ [!current ^ "\n"];
     current := ""

  let pr s = if (!mode = ML || !mode = MLsilent) then emit s
}

rule lex = parse
  eof { }
| "#{*}" { pr (Lexing.lexeme lexbuf); mode := ML; lex lexbuf  }
| "#{#}" { pr (Lexing.lexeme lexbuf); mode := MLsilent; lex lexbuf  }
| "#{@}" { mode := TEX; lex lexbuf }
| "(**" [^'\n']* '\n' { (match !mode with 
		ML | MLsilent -> () 
	      | _ -> pr (Lexing.lexeme lexbuf));
           lex lexbuf }
| "**)"  { (match !mode with 
		ML | MLsilent -> () 
	      | _ -> pr (Lexing.lexeme lexbuf));
           lex lexbuf }
| ";;;s" { pr ";;"; 
         (match !mode with ML | MLsilent -> dump true | _ -> ());
         lex lexbuf }
| ";;s" { pr ";;"; 
         (match !mode with ML | MLsilent -> dump false | _ -> ());
         lex lexbuf }
| ";;;" { pr ";;"; 
         (match !mode with ML | MLsilent -> dump true | _ -> ());
         lex lexbuf }
| ";;" { pr (Lexing.lexeme lexbuf); 
         (match !mode with ML | MLsilent -> dump false | _ -> ());
         lex lexbuf }
| _ { pr (Lexing.lexeme lexbuf); lex lexbuf  }

{
  let () =
    if Array.length Sys.argv == 3 then
      (wholename := Array.get Sys.argv 1;
       basename := Array.get Sys.argv 2;
       emit "(* \""; emit !wholename; emit "\" *)\n";
       lex (Lexing.from_channel (open_in !wholename)))
    else
      (basename := "ml/f"; 
       lex (Lexing.from_channel stdin));
    exit 0
}
