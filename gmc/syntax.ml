(* abstract syntax for judgments, rules and derivations *)

open MySupport.Pervasives

type id = string

type term = 
    Var of id
  | App of id * term list

type syndef = {
    mvars : id list;
    cat : id;
    body : term list
  }
   
type judgment = {
    pred : id;
    args : term list;
  }

type premise = 
    J of judgment
  | Qexp of string * string option
      (* quoted ML expression for a side condition with 
	 an optional TeX representation *)

type rule = {
    rname : string;
    rconc : judgment;
    rprem : premise list
  }

type game = {
    syndefs : syndef list;
    jdgdecls : (judgment * int) list;  (* integer denotes the number of inputs *)
    ruledefs : rule list;
    mldefs : string option
  }

let split_LCID s =
  let base = ignore (Str.string_match (Str.regexp "[a-z]+") s 0); Str.matched_string s in
  let suffix = 
    try
      ignore (Str.search_forward (Str.regexp "[0-9]+") s 0);
      Str.matched_string s
    with Not_found -> "" in
  let primes = 
    try
      ignore (Str.search_forward (Str.regexp "'+") s 0);
      Str.matched_string s
    with Not_found -> "" in
    (base, suffix, primes)

let base_LCID s = let (base, _, _) = split_LCID s in base

type decl =
      Category
    | MVar of id
    | TCon of id list * id (* term constructor with arity *)
    | JCon of id list * id list (* arities of inputs and outputs *)
    | IsA of id            (* subcategory *)


module VarSet = Set.Make(
  struct
    type t = id
    let compare = compare 
  end)

let rec fv_of_term = function
    Var id -> VarSet.singleton id
  | App(_, ts) ->
      List.fold_left (fun s t -> VarSet.union s (fv_of_term t)) VarSet.empty ts

module Env = 
struct
  open Format

  type t = (id * decl) list

  let rec print_ids = function
      [] -> ()
    | id :: [] -> print_string id
    | id :: rest -> printf "%s,@ " id; print_ids rest

  let rec print_binding id = function
      Category -> print_string id
    | MVar cat -> printf "%s: %s" id cat
    | TCon (argcats, cat) -> 
	begin
	  printf "%s: %s[@[" id cat; 
	  print_ids argcats; 
	  printf "@]]"
	end
    | JCon (incats, outcats) ->
	begin
	  printf "%s: j[@[" id; 
	  print_ids incats; 
	  print_string "; ";
	  print_ids outcats; 
	  printf "@]]"
	end
	
    | IsA cat -> printf "%s <: %s" id cat

  let rec print_env = function
      [] -> ()
    | (id, decl) :: [] -> print_binding id decl
    | (id, decl) :: rest ->
	print_env rest;
	printf ",@ ";
	print_binding id decl
      
  let rec lookup_cat env id = 
    match List.assoc id env with 
	MVar cat -> cat
      | _ -> raise Not_found

  let rec lookup_tcon env id = 
    match List.assoc id env with
	TCon (cats, cat) -> (cats, cat)
      | _ -> raise Not_found

  let rec lookup_jcon env id = 
    match List.assoc id env with
	JCon (incat, outcat) -> (incat, outcat)
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
	    (fun (Var x) -> 
	       let MVar cat = 
		 try List.assoc x env with 
		  Not_found -> failwith (x ^ " not found") 
	       in cat) vars 
	in of_body ((id, TCon (argcats, cat)) :: env) cat rest

  let rec of_jdg env = function
      [] -> env
    | (jdg, in_num) :: rest ->
	let argcats = (* categories of arguments of the judgment *)
	  List.map (fun (Var x) -> lookup_cat env (base_LCID x)) jdg.args in
	let in_cats = take in_num argcats 
	and out_cats = drop in_num argcats in
	  of_jdg ((jdg.pred, JCon (in_cats, out_cats)) :: env) rest 

  let of_game g =
    let initial_env =
      (* boolean values are specially treated *)
      [("true", MVar "bool"); ("false", MVar "bool")] in
    let catdecls = 
      List.fold_left 
	(fun env def -> (def.cat, Category)::env) initial_env g.syndefs in
    let mvardecls =
      List.fold_left 
	(fun env def -> 
	  let cat = MVar def.cat in
	    List.fold_left
	      (fun env' mvar -> (mvar, cat)::env') 
	      env def.mvars)
	catdecls g.syndefs in
    let constrdecls =
      List.fold_left
	(fun env def -> of_body env def.cat def.body) mvardecls g.syndefs 
    in
      of_jdg constrdecls g.jdgdecls

end
