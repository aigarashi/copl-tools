module Error :
  sig
    type pos = Lexing.position
    type 'a with_pos = { p : pos; v : 'a; }
    val print_pos : pos -> unit
    val print_2pos : pos -> pos -> unit
    val err : string -> unit
    val errAt : pos -> string -> unit
    val errBtw : pos -> pos -> string -> unit
    val warning : string -> unit
  end
module Pervasives :
  sig
    val take : int -> 'a list -> 'a list
    val drop : int -> 'a list -> 'a list
    val pop : int -> 'a Stack.t -> 'a list -> 'a list
  end
