(*
 * ML lex script to convert .src files to .tex files.
 *)

{
  (*
     MLVERB : inside #{*}{opt} ... #{@}
                 Print text vebatim (feed it to the checker)
     TEX : plain text
     CHARS : inside @
     VERB : inside #{&} ... #{@}  Print text verbatim
     #<-: insert the last output from the checker here
  *)
  type mode = ML | MLVERB | TEX | CHARS | VERB

  let mode = ref TEX
  let newline = ref true
  let firstline = ref true

  let inc intref = intref := !intref + 1
  let basename = ref ""
  let wholename = ref ""
  let count = ref 0
  let linenum = ref 1
  let filename_of c = !basename ^ "." ^ (string_of_int c)

  let pr s =
    if !mode <> ML then print_string s;
    newline := false

  let pc = function
      '\n' ->
	if !mode = CHARS then
	  begin
	    prerr_string "line ";
	    prerr_int (!linenum);
	    prerr_string ":\nNewline within @...@ sequence\n";
	    exit 1;
	  end
	else
	  begin
	    pr "\n";
	    newline := true;
	    firstline := false;
	    inc linenum;
	  end
    | c -> if !mode = CHARS then
	   (match c with
	     '_' -> pr "{\\char'137}"
	   | '#' -> pr "{\\char'43}"
	   | '$' -> pr "{\\char'44}"
	   | '%' -> pr "{\\char'45}"
	   | '&' -> pr "{\\char'46}"
	   | '~' -> pr "{\\char'176}"
	   | '^' -> pr "{\\char'136}"
	   | '\\' -> pr "{\\char'134}"
	   | '{' -> pr "{\\char'173}"
	   | '}' -> pr "{\\char'175}"
(*	   | '\'' -> pr "{\\ensuremath{'}}" *)
	   | _ -> print_char c)
      else
	  begin
	    (match !mode with
		ML -> ()
	      | MLVERB -> if c = '$' then pr "{\\$}" else print_char c
	      | _ -> print_char c);
	    newline:=false
	  end
}

rule lex = parse
  eof { }
| "#{*}" [^'\n']* '\n' {
    pr "\\begin{progeg}\n";
    mode := MLVERB; newline:=true; firstline := true; inc linenum; lex lexbuf
  }
| "#{#}" [^'\n']* '\n' {
    mode := ML; newline:=true; firstline := true; inc linenum; lex lexbuf
  }
| "#{&}" [' ' '\t']* '\n' {
    pr "\\begin{progeg}\n";
    mode := VERB; newline:=true; firstline := true; inc linenum; lex lexbuf
  }
| "#{@}" [' ' '\t']* '\n' {
    if (!mode = MLVERB) || (!mode = VERB) then print_string "\\end{progeg}\n";
    mode := TEX; newline:=false; inc linenum; lex lexbuf
  }
| "#<-" [' ' '\t']* '\n' ['\n']? {
    inc count;
    pr (Printf.sprintf "\\begin{progeg}\\showout{drv/%s}\n\\end{progeg}\n"
	   (filename_of !count));
    newline := true; firstline := true; inc linenum;
    lex lexbuf
  }
| "#<" [' ' '\t']* '\n' ['\n']? {
    inc count;
    pr (Printf.sprintf "\\input{drv/%s.out}\n"
	   (filename_of !count));
    newline := true; firstline := true; inc linenum;
    lex lexbuf
  }
| "@@@" {
   (match !mode with
       CHARS -> (pc '@'; mode := TEX; pr "}}")
     | ML -> ()
     | VERB | MLVERB -> pr "@@@"
     | TEX -> (mode := CHARS; pr "\\ensuremath{\\itbox{"; pc '@'));
   lex lexbuf
}
| "@@" {
   (match !mode with
      CHARS | TEX -> pc '@'
     | ML -> ()
     | MLVERB | VERB -> pr "@@");
   lex lexbuf
}
| '@' {
    (match !mode with
       CHARS -> (mode := TEX; pr "}}")
     | VERB | MLVERB -> pc '@'
     | ML -> ()
     | TEX -> (mode := CHARS; pr "\\ensuremath{\\itbox{"));
    lex lexbuf
  }
| "\\\\" {
    (match !mode with
    | MLVERB | TEX | ML -> pr "\\\\"
    | VERB -> pr "\\bslash{}"
    | CHARS -> pc '\\'; pc '\\');
    lex lexbuf
}
| '\\' {  (* single backslash *)
    (match !mode with
	MLVERB | ML -> pr "\\bslash{}"
      | VERB | TEX -> pr "\\"
      | CHARS -> pc '\\');
    lex lexbuf
}
| "->" {
   (match !mode with VERB | CHARS -> pr "\\(\\rightarrow\\)" | _  -> pr "->");
   lex lexbuf
}
| "\\{" {
   (match !mode with VERB -> pr "\\curlyopen{}" | _  -> pc '\\'; pc '{');
   lex lexbuf
}
| "\\}" {
   (match !mode with VERB -> pr "\\curlyclose{}" | _ -> pc '\\'; pc '}');
   lex lexbuf
}
| '{' {
   (match !mode with MLVERB -> pr "\\curlyopen{}"; | _  -> pc '{');
   lex lexbuf
}
| '}' {
   (match !mode with MLVERB -> pr "\\curlyclose{}" | _ -> pc '}');
   lex lexbuf
}
(* This pattern is to insert a narrower space after, e.g., '+.' *)
| ['.' ':' ';'] ' ' {
  pc (Lexing.lexeme_char lexbuf 0);
  (match !mode with CHARS -> pr "\\ " | _ -> pc ' ');
  lex lexbuf
}
| _ {
    pc (Lexing.lexeme_char lexbuf 0); lex lexbuf
  }

{
  let () =
    basename := "tex/hoge";
    Arg.parse
      ["-basename", Arg.String (fun s -> basename := s),
	"basename of the output files"]
      (fun s -> wholename := s)
      "Usage:";
    if !wholename = "" then
      begin
	lex (Lexing.from_channel stdin)
      end
    else
      begin
	pr "%% \""; pr !wholename; pr "\"\n";
	lex (Lexing.from_channel (open_in !wholename))
      end;
    exit 0
}
