open Format

module Error =
struct

  open Lexing
  type pos = position

  type 'a with_pos = {p:pos; v:'a}

  let print_pos pos =
    printf "File \"%s\", line %d, character %d" 
      pos.pos_fname
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol)

  let print_2pos pos1 pos2 =
    printf "File \"%s\", %d.%d--%d.%d" 
      pos1.pos_fname
      pos1.pos_lnum
      (pos1.pos_cnum - pos1.pos_bol)
      pos2.pos_lnum
      (pos2.pos_cnum - pos2.pos_bol)

  let errAt pos s =
    print_newline();
    print_pos pos;
    print_newline();
    print_string s;
    print_newline();
    exit 2

  let errBtw pos1 pos2 s =
    print_newline();
    print_2pos pos1 pos2;
    print_newline();
    print_string s;
    print_newline();
    exit 2

  let err s =
    print_newline ();
    print_string s;
    print_newline ();
    exit 2

  let warning s = 
    print_newline ();
    print_string "Warning: ";
    print_string s;
    print_newline ()

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
end
