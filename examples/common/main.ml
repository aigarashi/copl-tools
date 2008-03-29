open Lexing

let commandname = "checker"

let filename = ref ""  (* the name of the input file *)
let gname = ref ""     (* the name of a game *)
let jdg = ref ""       (* the judgment to prove *)

let fullp = ref false  (* controls whether full derivations are shown *)
let texp = ref false   (* controls whether output is in TeX *)

let spec = [
    ("-full", Arg.Set fullp, "Display full derivations");
    ("-TeX", Arg.Set texp, "output in TeX");
    ("-game", Arg.Set_string gname, "the name of a game");
    ("-prove", Arg.Set_string jdg, "the judgment to prove");
  ]

let games = [
    ("nat", (Nat.Link.process_derivation, Nat.Link.process_partial_jdg));
(*    ("ML1", Ml1.Link.process_derivation); *)
  ]

let () = 
  Arg.parse spec (fun s -> filename := s) 
    (Printf.sprintf "Usage: %s -game gamename [-full] [-TeX] filename\n%s -game gamename -prove judgment" 
	commandname commandname);

  if !gname = "" then failwith "Game name must be given"
  else
    begin
      let process_deriv, process_partial_jdg = 
	try List.assoc !gname games with 
	    Not_found -> failwith ("No such game: " ^ !gname) in
	if !jdg = "" then (* checker mode *)
	  begin
	    let lexbuf = Lexing.from_channel (open_in !filename) in
	    
	    let pos = lexbuf.lex_curr_p in
	      lexbuf.lex_curr_p <- { pos with pos_fname = !filename };
	      
	      while true do
		process_deriv lexbuf !fullp !texp;
	      done
	  end
	    
	else (* -prove mode*)
	  let lexbuf = Lexing.from_string !jdg in
	    process_partial_jdg lexbuf !fullp !texp
    end
