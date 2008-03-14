(* abstract syntax for judgments, rules and derivations *)

type id = string

type term = 
    Var of id
  | App of id * term list

type syndef = {
    mvar : id;
    cat : id;
    body : term list
  }
   
type judgment = {
    pred : id;
    args : term list
  }

type premise = 
    J of judgment
  | Qexp of string  (* quoted ML expression for a side condition *)

type rule = {
    rname : string;
    rconc : judgment;
    rprem : premise list
  }

type game = {
    syndefs : syndef list;
    jdgdecls : judgment list;
    ruledefs : rule list
  }

let split_LCID s =
  try 
    let pos = Str.search_forward (Str.regexp "[0-9 _ ']+") s 0 in
      Str.string_before s pos
  with Not_found -> s

type decl =
      Category
    | MVar of id
    | TCon of id list * id (* arity *)
    | IsA of id            (* subcategory *)

module Env = 
struct
  type t = (id * decl) list

  let rec lookup_cat env id = 
    match List.assoc id env with 
	MVar cat -> cat
      | _ -> raise Not_found

  let rec lookup_con env id = 
    match List.assoc id env with
	TCon (cats, cat) -> (cats, cat)
      | _ -> raise Not_found

  let rec is_subcat env cat1 cat2 = match env with
      [] -> false
    | (c, IsA c') :: rest when c = cat1 && c' = cat2 -> true
    | _ :: rest -> is_subcat rest cat1 cat2
	
  let rec of_body env cat = function
      [] -> env
    | Var id :: rest -> 
	let cat' = lookup_cat env id in
	  of_body ((cat', IsA cat) :: env) cat rest
    | App (id, vars) :: rest ->
	let argcats = 
	  List.map 
	    (fun (Var x) -> let MVar cat = 
	      try List.assoc x env with Not_found -> failwith (x ^ " not found") in cat) vars in
	  of_body ((id, TCon (argcats, cat)) :: env) cat rest

  let rec of_jdg env = function
      [] -> env
    | jdg :: rest ->
	let argcats = (* categories of arguments of the judgment *)
	  List.map (fun (Var x) -> lookup_cat env (split_LCID x)) jdg.args in
	  of_jdg ((jdg.pred, TCon (argcats, "j")) :: env) rest 

  let of_game g =
    let catdecls = 
      List.fold_left 
	(fun env def -> (def.cat, Category)::env) [] g.syndefs in
    let mvardecls =
      List.fold_left 
	(fun env def -> (def.mvar, MVar def.cat)::env) catdecls g.syndefs in
    let constrdecls =
      List.fold_left
	(fun env def -> of_body env def.cat def.body) mvardecls g.syndefs 
    in
      of_jdg constrdecls g.jdgdecls

end
