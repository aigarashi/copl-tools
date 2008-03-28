open Syntax
open Format

let pf = printf

let rec emit_seq ?(spbefore=true) delim emit_elm = function 
    (* spbefore = true means "insert space before delim" *)
    [] -> ()
  | e::rest -> emit_elm e; emit_seq2 spbefore delim emit_elm rest
and emit_seq2 spbefore delim emit_elm = function
    [] -> ()
  | e::rest -> 
      if spbefore then pf "@ %s " delim else pf "%s@ " delim; 
      emit_elm e; emit_seq2 spbefore delim emit_elm rest

let emit_comseq emit_elm = emit_seq ~spbefore:false "," emit_elm
let emit_barseq emit_elm = emit_seq "|" emit_elm

let emit_var env = function
    Var id -> print_string (String.lowercase (Env.lookup_cat env id))

module TypeDef = 
struct
  let rec emit_term this env = function
      Var id -> 
	(try let cat = Env.lookup_cat env id in 
	      pf "%s_of_%s of %s" this cat (String.lowercase cat)
	  with Not_found -> failwith ("emit_term: " ^ id ^ " not found"))
    | App (id, []) -> print_string id
    | App (id, ts) -> pf "%s of " id; 
 	              emit_seq "*" (emit_var env) ts

  let rec emit env = function
      [] -> failwith "Empty syntax definition"
    | sdef :: rest -> match sdef.body with
	  [] -> emit env rest
	| _ -> 
	    pf "@[type %s @[= " (String.lowercase sdef.cat); 
	    emit_barseq (emit_term sdef.cat env) sdef.body;
	    pf "@]@]@ ";
	    emit_typedef2 env rest;
  and emit_typedef2 env = function
      [] -> ()
    | sdef :: rest -> match sdef.body with
	  [] -> emit_typedef2 env rest
	| _ -> 	
	    pf "@[and %s @[= " (String.lowercase sdef.cat); 
	    emit_barseq (emit_term sdef.cat env) sdef.body;
	    pf "@]@]@\n";
	    emit_typedef2 env rest
end

module JdgDef = 
struct 
  let emit env jdgs =
    pf "@[type judgment @[= ";
    let emit_jdg env = function
	({pred = pred; args = []}, _) -> print_string pred
      | (jdg, _) ->
	  let ts = 
	    List.map 
	      (fun (Var v) -> Var (Syntax.base_LCID v)) jdg.args in
	    pf "%s of @[" jdg.pred;
	    emit_seq "*" (emit_var env) ts;
	    pf "@]"
    in
      emit_barseq (emit_jdg env) jdgs;
      pf "@]@]@ "
end

module Rules = 
struct
  let emit_term n tbl env cat term = 
    let incr id = 
      let m = try Hashtbl.find tbl id with Not_found -> 0 in 
	Hashtbl.replace tbl id (m+1); 
	m 
    in
    let rec aux (cat, term) = match term with
	Var id -> 
	  let prefix = String.make (incr id + n) '_' in 
	  let cat' = 
	    try Syntax.Env.lookup_cat env (Syntax.base_LCID id) with
		Not_found -> failwith ("emit_term: " ^ id ^ " not found") 
	  in
	    if cat' = cat then
	      pf "%s%s" prefix id
	    else if Syntax.Env.is_subcat env cat' cat 
	    then pf "%s_of_%s %s%s" cat cat' prefix id
	    else 
	      failwith 
		("emit_term:" ^ cat ^ " is not a sub category of " ^ cat')
      | App (id, []) -> 
	  let (_, cat') = 
	    try Syntax.Env.lookup_tcon env id with
		Not_found -> failwith ("emit_term: " ^ id ^ " not found")
	  in
	    if cat' = cat then pf "%s" id
	    else if Syntax.Env.is_subcat env cat' cat
	    then pf "%s_of_%s %s" cat cat' id
	    else 
	      failwith 
		("emit_term:" ^ cat ^ " is not a sub category of " ^ cat')
      | App (id, ts) -> 
	  let (cats, cat') = 
	    try Syntax.Env.lookup_tcon env (Syntax.base_LCID id) with
		Not_found -> failwith ("emit_term: " ^ id ^ " not found") 
	  in
	    if cat' = cat then
	      let ts = List.map2 (fun x y -> (x, y)) cats ts in 
	      pf "%s(@[" id; emit_comseq aux ts; pf "@])" 
    in
      aux (cat, term)

  let emit_eqs (tbl : (string, int) Hashtbl.t) = 
    let rec aux id m = 
      if m > 1 then 
	begin
	  pf "%s%s = %s &&@ " (String.make (m-1) '_') id id;
	  aux id (m-1)
	end
    in
    Hashtbl.iter aux tbl; pf "true"

  let emit_eqs' (tbl : (string, int list) Hashtbl.t) = 
    let rec aux id indices = 
      match indices with
	  [] -> failwith ("emit_eqs': no occurrence of variable " ^ id)
	| i :: rest ->
	    let rec aux' = function
		[] -> ()
	      | j :: rest -> 
		  pf "%s%s = %s%s &&@ " 
		    (String.make i '_') id 
		    (String.make j '_') id;
		  aux' rest
	    in aux' rest
    in
      pf "@[";
      Hashtbl.iter aux tbl;
      pf "true@]"
      
  let emit_jdg n tbl env = function
      {pred = pred; args = []} -> pf "%s" pred
    | jdg ->
	try 
	  let (incats, outcats) = Syntax.Env.lookup_jcon env jdg.pred in
	  let cats = incats @ outcats in
	  let ts = List.map2 (fun x y -> (x, y)) cats jdg.args in 
	    pf "%s(@[" jdg.pred; 
	    emit_comseq (fun (cat, t) -> emit_term n tbl env cat t) ts; 
	    pf "@])"
	with Not_found -> failwith ("emit_jdg: " ^ jdg.pred ^ " not found")

  let emit_pat_of_rule rname =
    pf "@[<2>{@[conc = _conc_;@ by = \"%s\";@ since = _derivs_;@ pos = _p_@]}@]" rname

  let emit_pat_of_jdg i env jdg = 
    let tbl = Hashtbl.create 50 in
    pf "@[  ";
    emit_jdg i tbl env jdg;
    pf "@ when @["; emit_eqs tbl; pf "@]@]";
    tbl

  let emit_pat_of_derivs n =
    pf "@[@ @ [@[";
    for i = 1 to n do
      pf "_d%d_;@ " i
    done;
    pf "@]]@]"

  let merge_tables master local i =
    let aux id n = (* for each item (id, n) in local *)
      if n > 0 then  (* if id appears, then add the index i to the master*)
	let old = try Hashtbl.find master id with Not_found -> [] in
	Hashtbl.replace master id (i::old)
    in
    Hashtbl.iter aux local

  let rec emit_exp_of_premises i rn tbl env = function
      [] -> 
	begin 
	  pf "@[if @[";
	  emit_eqs' tbl; 
	  pf "@]@ then _conc_";
	  pf "@ else errAt _p_ \"Wrong rule application: %s\"@]" rn
	end
    | J prem :: rest ->
	pf "@[(";
	begin 
	  pf "@[<v>@[match deriv_check _d%d_ with@]@ " i;
	  pf "@[<4>";
	  (* pat -> *) 
	  pf "@[  ";
	  merge_tables tbl (emit_pat_of_jdg i env prem) i;
	  pf " ->@]@ ";
	  (* exp *) emit_exp_of_premises (i+1) rn tbl env rest;
	  pf "@]@ ";
	  pf "@[| _ -> errAt _p_ \"The form of premise is wrong: %s\"@]@]" rn;
	end;
	pf ")@]"
    | Qexp s :: rest ->
	let b = Buffer.create (String.length s + 10) in
	let subst s = 
	  try 
	    match Hashtbl.find tbl s with 
		i :: _ -> (String.make i '_') ^ s
	      | _ -> raise Not_found
	  with Not_found -> failwith ("emit_exp_of_premises: " ^s ^ "doesn't appear in preceding premises")
	in
	  Buffer.add_substitute b subst s;
	  pf "@[if @[";
	  print_string (Buffer.contents b);
	  pf "@]@ then ";
	  emit_exp_of_premises (i+1) rn tbl env rest;
	  pf "@ else errAt _p_ \"Wrong rule application: %s\"@]" rn

  let rec count_jdg = function
      [] -> 0
    | J _ :: rest -> 1 + count_jdg rest
    | _ :: rest -> count_jdg rest

  let emit_clause_of_rule env r =
    pf "| @[<4>";
    emit_pat_of_rule r.rname;
    pf " ->@ ";
    pf "@[(";
    begin
      pf "@[<v>@[match _conc_ with@]@ ";
      pf "@[<2>"; 
      let tbl = Hashtbl.create 50 in  (* for recording variable occurrences *)
        merge_tables tbl (emit_pat_of_jdg 0 env r.rconc) 0; 
        pf " ->@ ";
        pf "@[(";
        begin
  	  pf "@[<v>@[match _derivs_ with@]@ ";
	  pf "@[<2>";
	  emit_pat_of_derivs (count_jdg r.rprem); 
	  pf " ->@ ";
	  begin
	    emit_exp_of_premises 1 r.rname tbl env r.rprem
	  end;
	  pf "@]@ ";
	  pf "@[| _ -> errAt _p_ \"The number of premises is wrong: %s\"@]@]" r.rname;
	end;
	pf ")@]@]@ ";
	pf "@[| _ -> errAt _p_ \"The form of conclusion is wrong: %s\"@]@]" r.rname
    end;
    pf ")@]@]"

  let emit env rules = 
    let rec loop = function
	[] -> pf "@[| {by=_name_; pos=_p_} -> errAt _p_ (\"No such rule: \" ^ _name_)@]"
      | rule::rest ->
	  emit_clause_of_rule env rule; pf "@ ";
	  loop rest
    in
      pf "@[<v>@[let rec deriv_check = function@]@ ";
      loop rules;
      pf "@]"

  module TeX =
    struct
      let pr = fprintf

      let rec emit_term ppf t = match t with
	  Var x -> 
	    let (base, suffix) = Syntax.split_LCID x in
	      pr ppf "\\metavar@[{%s_{%s}}@]" base suffix
	| App (f, ts) -> pr ppf "\\%sTerm@[{%a}@]" f emit_terms ts
      and emit_terms ppf = function
	  [] -> ()
	| t :: [] -> emit_term ppf t
	| t :: rest -> pr ppf "%a}@,{%a" emit_term t emit_terms rest

      let emit_qexp ppf s =
	let b = Buffer.create 30 in
	  Buffer.add_char b '(';
	  Buffer.add_substitute b 
	    (fun s -> 
	       let (base, suffix) = Syntax.split_LCID s in
		 "\\metavar{" ^ base ^ "_{" ^ suffix ^ "}}")
	    s;
	  Buffer.add_char b ')';
	  pr ppf "%s" (Buffer.contents b)

      let rec emit_jdg ppf j = 
	pr ppf "\\%s{%a}" j.pred emit_terms j.args
      and emit_premises ppf = function
	  [] -> ()
	| J j :: [] -> emit_jdg ppf j
	| Qexp q :: [] -> emit_qexp ppf q
	| J j :: rest -> pr ppf "%a@ \\andalso@ %a" emit_jdg j emit_premises rest
	| Qexp q :: rest -> pr ppf "%a@ \\andalso@ %a" emit_qexp q emit_premises rest
	
      let emit ppf r =
	pr ppf "@[\\infrule[%s]{%a}{%a}@]" 
	  r.rname 
	  emit_premises r.rprem
	  emit_jdg r.rconc
	  
    end
end

let typedef = TypeDef.emit
and jdgdef = JdgDef.emit
and rules = Rules.emit

let tex_rules rules = 
  List.iter (fun r -> Rules.TeX.emit std_formatter r) rules


