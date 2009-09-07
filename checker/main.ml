open Lexing
open MySupport.Error
open MySupport.Pervasives

let commandname = "checker"

let filenames = ref []  (* the name of the input file *)
let gname = ref ""     (* the name of a game *)
let jdg = ref ""       (* the judgment to prove *)
let deriv = ref ""     (* the derivation to check *)
let concl = ref ""     (*  against this judgment *)

let fullp = ref false  (* controls whether full derivations are shown *)
let texp = ref false   (* controls whether output is in TeX *)

let spec = [
    ("-full", Arg.Set fullp, "Display full derivations");
    ("-check", Arg.Set_string deriv, "Derivation to check");
    ("-against", Arg.Set_string concl, "Expected conclusion");
    ("-TeX", Arg.Set texp, "Output in TeX");
    ("-game", Arg.Set_string gname, "Specify the name of a game");
    ("-prove", Arg.Set_string jdg, "Proving mode; followed by the judgment to prove");
  ]

let games = [
    ("Nat", (Nat.Link.check_deriv, Nat.Link.make_deriv));
    ("NatLt1", (NatLt1.Link.check_deriv, NatLt1.Link.make_deriv));
    ("NatLt2", (NatLt2.Link.check_deriv, NatLt2.Link.make_deriv));
    ("NatLt3", (NatLt3.Link.check_deriv, NatLt3.Link.make_deriv));
    ("NatExp", (NatExp.Link.check_deriv, NatExp.Link.make_deriv));
    ("ML1", (Ml1.Link.check_deriv, Ml1.Link.make_deriv));
    ("ML2", (Ml2.Link.check_deriv, Ml2.Link.make_deriv));
    ("ML3", (Ml3.Link.check_deriv, Ml3.Link.make_deriv));
    ("ML4", (Ml4.Link.check_deriv, Ml4.Link.make_deriv));
    ("ContML4", (ContML4.Link.check_deriv, ContML4.Link.make_deriv));
    ("RefML4", (RefML4.Link.check_deriv, RefML4.Link.make_deriv));
    ("ML5", (Ml5.Link.check_deriv, Ml5.Link.make_deriv));
    ("ML6", (Ml6.Link.check_deriv, Ml6.Link.make_deriv));
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
		while true do
		  check_deriv lexbuf ?against !fullp !texp;
		done;
		loop rest
	  in 
	  let lexbufs = 
	    if !deriv = "" then (* read from files / stdin *)
	      match !filenames with
		  [] -> [Lexing.from_channel stdin]
		| _ -> List.rev_map make_lexbuf !filenames 
	    else [Lexing.from_string !deriv]
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
