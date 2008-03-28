type id = string

type term = Var of id | App of id * term list

type syndef = { mvar : id; cat : id; body : term list; }
type judgment = { pred : string; args : term list }

type premise = 
    J of judgment
  | Qexp of string  (* quoted ML expression for a side condition *)

type rule = { rname : string; rconc : judgment; rprem : premise list; }

type game = {
  syndefs : syndef list;
  jdgdecls : (judgment * int) list;
  ruledefs : rule list;
}

val split_LCID : string -> (string * string)

val base_LCID : string -> string

type decl = 
    Category 
  | MVar of id 
  | TCon of id list * id
  | JCon of id list * id list
  | IsA of id

module Env :
  sig
    type t = (id * decl) list

    val print_env : t -> unit
    val lookup_cat : t -> id -> id
    val lookup_tcon : t -> id -> id list * id
    val lookup_jcon : t -> id -> id list * id list
    val is_subcat : t -> id -> id -> bool
    val of_body : t -> id -> term list -> t
    val of_jdg : t -> (judgment * int) list -> t
    val of_game : game -> t
  end
