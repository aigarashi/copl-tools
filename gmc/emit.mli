open Syntax

val emit_seq : 
  ?spbefore:bool -> string -> 
  (Format.formatter -> 'a -> 'b) -> Format.formatter -> 'a list 
  -> unit
val emit_comseq : (Format.formatter -> 'a -> 'b) -> Format.formatter -> 'a list -> unit
val emit_barseq : (Format.formatter -> 'a -> 'b) -> Format.formatter -> 'a list -> unit
val emit_var : Env.t -> Format.formatter -> term -> unit

module TypeDef :
  sig
    val emit : Env.t -> Format.formatter -> syndef list -> unit
  end

module JdgDef :
  sig 
    val emit : Env.t -> Format.formatter -> (judgment * int) list -> unit 
  end

module Rules :
  sig
    val emit_term :
      int ->
      (id, int) Hashtbl.t ->
      (id * decl) list -> id -> Format.formatter -> term -> unit
    val emit_eqs : int -> Format.formatter -> (string, int) Hashtbl.t -> unit
    val emit_eqs' : Format.formatter -> (string, int list) Hashtbl.t -> unit
    val emit_jdg :
      int ->
      (id, int) Hashtbl.t ->
      (id * decl) list -> Format.formatter -> judgment -> unit
    val emit_pat_of_rule : Format.formatter -> string -> unit
    val emit_pat_of_jdg :
      int ->
      (id * decl) list ->
      Format.formatter -> judgment -> (id, int) Hashtbl.t
    val emit_pat_of_derivs : Format.formatter -> Syntax.premise list -> unit
    val merge_tables :
      ('a, 'b list) Hashtbl.t -> ('a, int) Hashtbl.t -> 'b -> unit
    val emit_exp_of_premises :
      int ->
      string ->
      (id, int list) Hashtbl.t ->
      (id * decl) list -> Format.formatter -> premise list -> unit
    val emit_clause_of_rule :
      (id * decl) list -> Format.formatter -> rule -> unit
    val emit : (id * decl) list -> Format.formatter -> rule list -> unit
  end

val typedef : Env.t -> Format.formatter -> syndef list -> unit
val jdgdef : Env.t -> Format.formatter -> (judgment * int) list -> unit
val rules : Env.t -> Format.formatter -> rule list -> unit
val tex_rules : rule list -> unit

module Prover :
sig
  val emit_jdgdef : Env.t -> Format.formatter -> (judgment * int) list -> unit
  val emit : Env.t -> Format.formatter -> rule list -> unit
end
