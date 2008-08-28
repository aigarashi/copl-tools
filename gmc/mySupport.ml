open Printf

module Error =
struct

  open Lexing
  type pos = position

  type 'a with_pos = {p:pos; v:'a}

  let print_pos pos =
    eprintf "File \"%s\", line %d, character %d" 
      pos.pos_fname
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol)

  let print_2pos pos1 pos2 =
    eprintf "File \"%s\", %d.%d--%d.%d" 
      pos1.pos_fname
      pos1.pos_lnum
      (pos1.pos_cnum - pos1.pos_bol)
      pos2.pos_lnum
      (pos2.pos_cnum - pos2.pos_bol)

  let err s =
    eprintf "\n%s\n" s;
    exit 2

  let errAt pos s =
    eprintf "\n";
    print_pos pos;
    err s

  let errBtw pos1 pos2 s =
    eprintf "\n";
    print_2pos pos1 pos2;
    err s

  let warning s = 
    eprintf "\n";
    eprintf "Warning: %s\n" s

end

module Pervasives =
struct
  open Buffer
  let rec take n l =
    if n = 0 then []
    else match l with [] -> [] | a :: rest -> a :: take (n-1) rest

  let rec drop n l = 
    if n = 0 then l
    else match l with [] -> [] | a :: rest -> drop (n-1) rest

  let rec pop n s acc =
    if n = 0 then acc
    else let top = Stack.pop s in pop (n-1) s (top :: acc)

  let iteri f l =
    let rec loop n l =
      match l with
	  [] -> ()
	| x :: l' -> f n x; loop (n+1) l'
    in loop 1 l

  let rec pos a l = 
    match l with
	[] -> raise Not_found
      | a' :: rest -> if a = a' then 1 else 1 + pos a rest

  (* adapted from Buffer *)
  let closing = function
    | '(' -> ')'
    | '{' -> '}'
    | _ -> assert false;;

  (* opening and closing: open and close characters, typically ( and )
     k: balance of opening and closing chars
     s: the string where we are searching
     start: the index where we start the search. *)
  let advance_to_closing opening closing k s start =
    let rec advance k i lim =
      if i >= lim then raise Not_found else
	if s.[i] = opening then advance (k + 1) (i + 1) lim else
	  if s.[i] = closing then
	    if k = 0 then i else advance (k - 1) (i + 1) lim
	  else advance k (i + 1) lim in
      advance k start (String.length s);;

  let advance_to_non_alpha s start =
    let rec advance i lim =
      if i >= lim then lim else
	match s.[i] with
	  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' |
		'�'|'�'|'�'|'�'|'�'|'�'|'�'|
		    '�'|'�'|'�'|'�'|'�'|'�'|'�'|
			'�'|'�'|'�'|'�'|'�'|'�'|'�'|
			    '�'|'�'|'�'|'�'|'�'|'�'|'�' ->
              advance (i + 1) lim
	  | _ -> i in
      advance start (String.length s);;

  (* We are just at the beginning of an ident in s, starting at start. *)
  let find_ident s start =
    match s.[start] with
	(* Parenthesized ident ? *)
      | '(' | '{' as c ->
	  let new_start = start + 1 in
	  let stop = advance_to_closing c (closing c) 0 s new_start in
	    String.sub s new_start (stop - start - 1), stop + 1
	      (* Regular ident *)
      | _ ->
	  let stop = advance_to_non_alpha s (start + 1) in
	    String.sub s start (stop - start), stop;;

  (* Substitute $ident, $(ident), or ${ident} in s,
     according to the function mapping f. *)
  let rec add_substitute b f s =
    let lim = String.length s in
    let rec subst previous i =
      if i < lim then begin
	  match s.[i] with
	    | '$' as current when previous = '\\' ->
		add_char b current;
		subst current (i + 1)
	    | '$' ->
		let ident, next_i = find_ident s (i + 1) in
		  add_string b (f ident);
		  subst ' ' next_i
	    | current when previous == '\\' ->
		add_char b '\\';
		(* Changed *)
		if current != '\\' then add_char b current;
		subst current (i + 1)
	    | '\\' as current ->
		(* Changed *)
		subst current (i + 1)
	    | current ->
		add_char b current;
		subst current (i + 1)
	end 
      else if previous == '\\' then add_char b previous in
      subst ' ' 0;;

      let escaped_for_Scheme s = (* adapted from String.escaped *)
	let n = ref 0 in
	  for i = 0 to String.length s - 1 do
	    n := !n +
              (match String.unsafe_get s i with
		  '"' | '\\' | '\n' | '\t' | ',' -> 2
		| _ -> 1) (* assuming non-printable character won't appear *)
	      (* | c -> if is_printable c then 1 else 4 *)
	  done;
	  if !n = String.length s then s else begin
	      let s' = String.create !n in
		n := 0;
		for i = 0 to String.length s - 1 do
		  begin
		    match String.unsafe_get s i with
			('"' | '\\') as c ->
			  String.unsafe_set s' !n '\\'; incr n; 
			  String.unsafe_set s' !n c
		      | ',' ->
			  String.unsafe_set s' !n ','; incr n; 
			  String.unsafe_set s' !n ','
		      | '\n' ->
			  String.unsafe_set s' !n '\\'; incr n; 
			  String.unsafe_set s' !n 'n'
		      | '\t' ->
			  String.unsafe_set s' !n '\\'; incr n; 
			  String.unsafe_set s' !n 't'
		      | c -> String.unsafe_set s' !n c
		  end;
		  incr n
		done;
		s'
	    end
end
