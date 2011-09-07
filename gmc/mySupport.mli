module Error :
  sig
    type pos = Lexing.position
    type 'a with_pos = { p : pos; v : 'a; }
    val print_pos : pos -> unit
    val print_2pos : pos -> pos -> unit
    val err : string -> 'a
    val errAt : pos -> string -> 'a
    val errBtw : pos -> pos -> string -> 'a
    val warning : string -> unit
    val warningAt : pos -> string -> unit
    val warningBtw : pos -> pos -> string -> unit
  end
module Pervasives :
  sig
    val take : int -> 'a list -> 'a list
    val drop : int -> 'a list -> 'a list
    val pop : int -> 'a Stack.t -> 'a list -> 'a list
    val iteri : (int -> 'a -> unit) -> 'a list -> unit
    val pos : 'a -> 'a list -> int
    val add_substitute : Buffer.t -> (string -> string) -> string -> unit
    val escaped_for_Scheme : string -> string
  end
