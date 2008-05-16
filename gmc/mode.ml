(* Mode Analysis:

For a rule of the form:

  J0 :- J1,...,Jn

and Ji = fi(t1,...,tn; s1,...,sm),

- output vars of J0 are free vars of t1,...tn
- output vars of Ji (1 <= i <= n) are free vars of s1,...,sm

- for 1 <= i <= n, all free vars of t1,...,tn must be included in the
   output vars from preceding judgments.

- all free vars of s1,...,sm must be included in the output vars from
  J0,J1,...,Jn.

*)

open MySupport.Pervasives
open MySupport.Error
open Syntax

let invars_of_jdg (env : Env.t) (jdg : judgment) : VarSet.t = 
  let innums = List.length (fst (Env.lookup_jcon env jdg.pred)) in
  let interms = take innums jdg.args in
    List.fold_left (fun s t -> VarSet.union s (fv_of_term t)) 
      VarSet.empty interms 

let outvars_of_jdg (env :Env.t) (jdg : judgment) : VarSet.t = 
  let outnums = List.length (fst (Env.lookup_jcon env jdg.pred)) in
  let outterms = drop outnums jdg.args in
    List.fold_left (fun s t -> VarSet.union s (fv_of_term t)) 
      VarSet.empty outterms 

let rec check_premises 
    (env : Env.t) (vars_given : VarSet.t) (prems : premise list) 
    : VarSet.t option =
  match prems with
      [] -> Some vars_given
    | J jdg :: rest ->
	let invars = invars_of_jdg env jdg in
	  if VarSet.is_empty (VarSet.diff invars vars_given) 
	    (* invars \subseteq vars_given *)
	  then
	    let newvars = VarSet.union (outvars_of_jdg env jdg) vars_given in
	      check_premises env newvars rest
	  else None
    | Qexp s :: rest -> (* assume a side condition is mode correct and 
			all variables in exp are instantiated here *)
	let b = Buffer.create (String.length s) in
	let vars = ref VarSet.empty in
	let subst s = 
	  vars := VarSet.add s !vars;
	  s
	in
	  Buffer.add_substitute b subst s;
	  check_premises env (VarSet.union vars_given !vars) rest

let check_rule (env : Env.t) (r : rule) : bool =
  let invars = invars_of_jdg env r.rconc in
  match check_premises env invars r.rprem with
      Some outvars -> 
	let conc_ovars = outvars_of_jdg env r.rconc in
	  VarSet.is_empty (VarSet.diff conc_ovars outvars)
    | None -> false

let rec check_rules (env : Env.t) (rs : rule list) (i : int) : bool =
  match rs with 
      [] -> true
    | r :: rest -> 
	if check_rule env r then check_rules env rest (i+1)
	else 
	  begin
	    warning ("mode check failure in " ^ string_of_int i ^"-th rule");
	    false
	  end

let check_rules (env : Env.t) (rs : rule list) : bool = check_rules env rs 1
