open Syntax

val emit_seq : ?spbefore:bool -> string -> ('a -> 'b) -> 'a list -> unit
val emit_comseq : ('a -> 'b) -> 'a list -> unit
val emit_barseq : ('a -> 'b) -> 'a list -> unit
val emit_var : Env.t -> term -> unit

module TypeDef :
  sig
    val emit : Env.t -> syndef list -> unit
  end

module JdgDef :
  sig val emit : Env.t -> judgment list -> unit end

module Rules :
  sig
    val emit_term :
      int ->
      (id, int) Hashtbl.t ->
      (id * decl) list -> id -> term -> unit
    val emit_eqs : (string, int) Hashtbl.t -> unit
    val emit_eqs' : (string, int list) Hashtbl.t -> unit
    val emit_jdg :
      int ->
      (id, int) Hashtbl.t ->
      (id * decl) list -> judgment -> unit
    val emit_pat_of_rule : string -> unit
    val emit_pat_of_jdg :
      int ->
      (id * decl) list ->
      judgment -> (id, int) Hashtbl.t
    val emit_pat_of_derivs : int -> unit
    val merge_tables :
      ('a, 'b list) Hashtbl.t -> ('a, int) Hashtbl.t -> 'b -> unit
    val emit_exp_of_premises :
      int ->
      string ->
      (id, int list) Hashtbl.t ->
      (id * decl) list -> judgment list -> unit
    val emit_clause_of_rule :
      (id * decl) list -> rule -> unit
    val emit : (id * decl) list -> rule list -> unit
  end

val typedef : Env.t -> syndef list -> unit
val jdgdef : Env.t -> judgment list -> unit
val rules : Env.t -> rule list -> unit
