open Lexing
open MySupport.Error
open MySupport.Pervasives

let commandname = "checker"

let filenames = ref []  (* the name of the input file *)
let gname = ref ""     (* the name of a game *)
let jdg = ref ""       (* the judgment to prove *)
let deriv = ref (None : string option)    (* the derivation to check *)
let concl = ref ""     (*  against this judgment *)

let fullp = ref false  (* controls whether full derivations are shown *)
let texp = ref false   (* controls whether output is in TeX *)

let spec = [
    ("-full", Arg.Set fullp, "Display full derivations");
    ("-check", Arg.String (fun s -> deriv := Some s),
    "Derivation to check");
    ("-against", Arg.Set_string concl, "Expected conclusion");
    ("-TeX", Arg.Set texp, "Output in TeX");
    ("-game", Arg.Set_string gname, "Specify the name of a game");
    ("-prove", Arg.Set_string jdg, "Proving mode; followed by the judgment to prove");
    ("-debug", Arg.Unit (fun () -> ignore (Parsing.set_trace true)), "Set the debugging flag of the parser");
  ]

let games = [
    ("Nat", (Nat.Link.check_deriv, Nat.Link.make_deriv));
    ("CompareNat1", (CompareNat1.Link.check_deriv, CompareNat1.Link.make_deriv));
    ("CompareNat2", (CompareNat2.Link.check_deriv, CompareNat2.Link.make_deriv));
    ("CompareNat3", (CompareNat3.Link.check_deriv, CompareNat3.Link.make_deriv));
    ("EvalNatExp", (EvalNatExp.Link.check_deriv, EvalNatExp.Link.make_deriv));
    ("ReduceNatExp", (ReduceNatExp.Link.check_deriv, ReduceNatExp.Link.make_deriv));
    ("EvalML1", (EvalML1.Link.check_deriv, EvalML1.Link.make_deriv));
    ("EvalML1Err", (EvalML1Err.Link.check_deriv, EvalML1Err.Link.make_deriv));
    ("EvalML2", (EvalML2.Link.check_deriv, EvalML2.Link.make_deriv));
    ("EvalML3", (EvalML3.Link.check_deriv, EvalML3.Link.make_deriv));
    ("EvalML4", (EvalML4.Link.check_deriv, EvalML4.Link.make_deriv));
    ("EvalML5", (EvalML5.Link.check_deriv, EvalML5.Link.make_deriv));
    ("EvalML6", (EvalML6.Link.check_deriv, EvalML6.Link.make_deriv));
    ("NamelessML3", (NamelessML3.Link.check_deriv, NamelessML3.Link.make_deriv));
    ("EvalNamelessML3", (EvalNamelessML3.Link.check_deriv, EvalNamelessML3.Link.make_deriv));
    ("EvalContML1", (EvalContML1.Link.check_deriv, EvalContML1.Link.make_deriv));
    ("EvalContML3", (EvalContML3.Link.check_deriv, EvalContML3.Link.make_deriv));
    ("EvalContML4", (EvalContML4.Link.check_deriv, EvalContML4.Link.make_deriv));
    ("EvalDContML4", (EvalDContML4.Link.check_deriv, EvalDContML4.Link.make_deriv));
    ("EvalRefML3", (EvalRefML3.Link.check_deriv, EvalRefML3.Link.make_deriv));
    ("EvalRefML4", (EvalRefML4.Link.check_deriv, EvalRefML4.Link.make_deriv));
    ("TypingML2", (TypingML2.Link.check_deriv, TypingML2.Link.make_deriv));
    ("TypingML3", (TypingML3.Link.check_deriv, TypingML3.Link.make_deriv));
    ("TypingML4", (TypingML4.Link.check_deriv, TypingML4.Link.make_deriv));
    ("TypingML6", (TypingML6.Link.check_deriv, TypingML6.Link.make_deriv));
    ("PolyTypingML3", (PolyTypingML3.Link.check_deriv, PolyTypingML3.Link.make_deriv));
    ("PolyTypingML4", (PolyTypingML4.Link.check_deriv, PolyTypingML4.Link.make_deriv));
    ("While", (While.Link.check_deriv, While.Link.make_deriv));
  ]

let () =
  Arg.parse spec (fun s -> filenames := s :: !filenames)
    (Printf.sprintf "\
Usage: %s -game gamename [-full] [-TeX] [-against conclusion] [filename ...]
       %s -game gamename [-full] [-TeX] -prove judgment" commandname commandname);

  if !gname = "" then err "Game name must be given."
  else
    begin
      let check_deriv, make_deriv =
	try List.assoc !gname games with
	    Not_found -> failwith ("No such game: " ^ !gname) in
	if !jdg = "" then (* checker mode *)
	  let against = if !concl = "" then None else Some !concl in
	  let make_lexbuf s =
	    let lexbuf = Lexing.from_channel (open_in s) in
	    let pos = lexbuf.lex_curr_p in
	      lexbuf.lex_curr_p <- { pos with pos_fname = s};
	      lexbuf
	  in
	  let rec loop lexbufs = match lexbufs with
	      [] -> ()
	    | lexbuf::rest ->
		try
		  while true do
		    check_deriv lexbuf ?against !fullp !texp
		  done
		with End_of_file -> ();
		loop rest
	  in
	    match !deriv with
		Some d ->
		  (* If a string is given by a command line, it should
		     be one derivation *)
		  (try
		      check_deriv (Lexing.from_string d) ?against !fullp !texp
		   with End_of_file -> err ("Empty input string."))
	      | None ->
		  (* If it is not given, a sequence of derivations may
		     be supplied from stdin or files *)
		  let lexbufs =
		    match !filenames with
			[] -> [Lexing.from_channel stdin]
		      | _ -> List.rev_map make_lexbuf !filenames
		  in loop lexbufs

	else (* -prove mode*)
	  let lexbuf = Lexing.from_string !jdg in
	    try
	      make_deriv lexbuf !fullp !texp
	    with
		Stack_overflow ->
		  err ("Couldn't find a derivation for " ^ !jdg ^
			 " within a reasonable amount of memory.")
    end
