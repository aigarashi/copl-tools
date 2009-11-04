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
    ("ContML1", (ContML1.Link.check_deriv, ContML1.Link.make_deriv));
    ("ContML4", (ContML4.Link.check_deriv, ContML4.Link.make_deriv));
    ("RefML4", (RefML4.Link.check_deriv, RefML4.Link.make_deriv));
    ("TypingML2", (TypingML2.Link.check_deriv, TypingML2.Link.make_deriv));
    ("TypingML4", (TypingML4.Link.check_deriv, TypingML4.Link.make_deriv));
    ("TypingML5", (TypingML5.Link.check_deriv, TypingML5.Link.make_deriv));
    ("PolyML4", (PolyML4.Link.check_deriv, PolyML4.Link.make_deriv));
    ("PolyML5", (PolyML5.Link.check_deriv, PolyML5.Link.make_deriv));
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
