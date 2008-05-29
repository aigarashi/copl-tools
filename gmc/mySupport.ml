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
end
