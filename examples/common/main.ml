open Lexing

let commandname = "checker"

let filename = ref ""  (* the name of the input file *)
let gname = ref ""     (* the name of a game *)

let fullp = ref false  (* controls whether full derivations are shown *)
let texp = ref false   (* controls whether output is in TeX *)

let spec = [
    ("-full", Arg.Set fullp, "Display full derivations");
    ("-TeX", Arg.Set texp, "output in TeX");
    ("-game", Arg.Set_string gname, "the name of a game");
  ]

let games = [
    ("nat", Nat.Link.process_input);
    ("ML1", Ml1.Link.process_input);
  ]

let () = 
  Arg.parse spec (fun s -> filename := s) 
    (Printf.sprintf "Usage: %s -game gamename [-full] [-TeX] filename" 
	commandname);

  if !gname = "" then failwith "Game name must be given"
  else
    begin
      let process_f = 
	try List.assoc !gname games with 
	    Not_found -> failwith ("No such game: " ^ !gname) in
      let lexbuf = Lexing.from_channel (open_in !filename) in

      let pos = lexbuf.lex_curr_p in
	lexbuf.lex_curr_p <- { pos with pos_fname = !filename };

	while true do
	  process_f lexbuf !fullp !texp;
	done
    end
