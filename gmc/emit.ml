open MySupport.Pervasives
open Syntax
open Format

let pf = fprintf

(* common emit functionals *)
let rec emit_seq ?(spbefore=true) delim emit_elm ppf = function 
    (* spbefore = true means "insert space before delim" *)
    [] -> ()
  | e::rest -> emit_elm ppf e; emit_seq2 spbefore delim emit_elm ppf rest
and emit_seq2 spbefore delim emit_elm ppf = function
    [] -> ()
  | e::rest -> 
      if spbefore then pf ppf "@ %s " delim else pf ppf "%s@ " delim; 
      emit_elm ppf e; emit_seq2 spbefore delim emit_elm ppf rest

let emit_comseq emit_elm ppf e = emit_seq ~spbefore:false "," emit_elm ppf e
let emit_barseq emit_elm ppf e = emit_seq "|" emit_elm ppf e
let emit_semiseq emit_elm ppf e = emit_seq ~spbefore:false ";" emit_elm ppf e

let emit_var env ppf = function
    Var id -> pp_print_string ppf (String.lowercase (Env.lookup_cat env id))

let emit_coercion env ppf (cat', cat) =
  if cat' <> cat then 
    if Syntax.Env.is_subcat env cat' cat
    then pf ppf "%s_of_%s " cat cat'
    else 
      failwith ("emit_coercion:" ^ cat ^ " is not a subcategory of " ^ cat')

let emit_ordinal ppf i = (* doesn't work for some numbers *)
  match i with
      1 -> pp_print_string ppf "first"
    | 2 -> pp_print_string ppf "second"
    | 3 -> pp_print_string ppf "third"
    | _ -> pf ppf "%d-th" i

module type EMITTER =
sig
  val of_bnf : Env.t -> Format.formatter -> syndef list -> unit
  val of_judgments : Env.t -> Format.formatter -> (judgment * int) list -> unit 
    (* integers specifies how many arguments are considered as input *)
  val of_rules : (id * decl) list -> Format.formatter -> rule list -> unit
end

module ML =
struct
  module TypeDef = 
  struct
    let rec emit_term this env ppf = function
	Var id -> 
	  (try let cat = Env.lookup_cat env id in 
	     pf ppf "%s_of_%s of %s" this cat (String.lowercase cat)
	   with Not_found -> failwith ("emit_term: " ^ id ^ " not found"))
      | App (id, []) -> print_string id
      | App (id, ts) -> pf ppf "%s of " id; 
 	  emit_seq "*" (emit_var env) ppf ts

    let rec of_bnf env ppf = function
	[] -> failwith "Empty syntax definition"
      | sdef :: rest -> match sdef.body with
	    [] -> of_bnf env ppf rest
	  | _ -> 
	      pf ppf "@[type %s @[= %a@]@]@ " 
		(String.lowercase sdef.cat)
		(emit_barseq (emit_term sdef.cat env)) sdef.body;
	      emit_typedef2 env ppf rest;
    and emit_typedef2 env ppf = function
	[] -> ()
      | sdef :: rest -> match sdef.body with
	    [] -> emit_typedef2 env ppf rest
	  | _ -> 	
	      pf ppf "@[and %s @[= %a@]@]@\n" (String.lowercase sdef.cat)
		(emit_barseq (emit_term sdef.cat env)) sdef.body;
	      emit_typedef2 env ppf rest
  end

  (* exported function *)
  let of_bnf = TypeDef.of_bnf

  (* exported function *)
  let of_judgments env ppf jdgs =
    let emit_jdg env ppf = function
	({pred = pred; args = []}, _) -> pp_print_string ppf pred
      | (jdg, _) ->
	  let ts = 
	    List.map 
	      (fun (Var v) -> Var (Syntax.base_LCID v)) jdg.args in
	    pf ppf "%s of @[%a@]" 
	      jdg.pred
	      (emit_seq "*" (emit_var env)) ts;
    in
      pf ppf "@[type judgment @[= %a@]@]@ "
	(emit_barseq (emit_jdg env)) jdgs

  module Rules = 
  struct
    (* emit a term as a pattern *)
    let emit_term n tbl env cat ppf term = 
      let incr id = 
	let m = try Hashtbl.find tbl id with Not_found -> 0 in 
	  Hashtbl.replace tbl id (m+1); 
	  m 
      in
      let rec aux ppf (cat, term) = match term with
	  (* boolean values are treated specially *)
	  Var ("true" | "false" as b) ->
	    pf ppf "%a%s" (emit_coercion env) ("bool", cat) b
	| Var id -> (* other variables *)
	    let prefix = String.make (incr id + n) '_' in 
	    let cat' = 
	      try Syntax.Env.lookup_cat env (Syntax.base_LCID id) with
		  Not_found -> failwith ("emit_term: " ^ id ^ " not found") 
	    in
	      pf ppf "%a%s%s" (emit_coercion env) (cat', cat) prefix id
	| App (id, ts) -> 
	    let (cats, cat') = 
	      try Syntax.Env.lookup_tcon env id with
		  Not_found -> failwith ("emit_term: " ^ id ^ " not found")
	    in
	      match ts with 
		  [] -> pf ppf "%a%s" (emit_coercion env) (cat', cat) id
		| _ -> 
		    let ts = List.map2 (fun x y -> (x, y)) cats ts in 
		      pf ppf "%a(%s(@[%a@]))" 
			(emit_coercion env) (cat', cat)
			id 
			(emit_comseq aux) ts
      in
	aux ppf (cat, term)

    let emit_eqs n ppf (tbl : (string, int) Hashtbl.t) = 
      let rec aux ppf id m = 
	if m > 1 then 
	  begin
	    pf ppf "%s%s = %s%s &&@ " 
	      (String.make n '_') id
	      (String.make (n+m-1) '_') id;
	    aux ppf id (m-1)
	  end
      in
	pf ppf "@[%atrue@]"
	  (fun ppf -> Hashtbl.iter (aux ppf)) tbl

    let emit_eqs' ppf (tbl : (string, int list) Hashtbl.t) = 
      let rec aux ppf id indices = 
	match indices with
	    [] -> failwith ("emit_eqs': no occurrence of variable " ^ id)
	  | i :: rest ->
	      let rec aux' = function
		  [] -> ()
		| j :: rest -> 
		    pf ppf "%s%s = %s%s &&@ " 
		      (String.make i '_') id 
		      (String.make j '_') id;
		    aux' rest
	      in aux' rest
      in
	pf ppf "@[%atrue@]" (fun ppf -> Hashtbl.iter (aux ppf)) tbl
	  
    let emit_jdg n tbl env ppf = function
	{pred = pred; args = []} -> pp_print_string ppf pred
      | jdg ->
	  try 
	    let (incats, outcats) = Syntax.Env.lookup_jcon env jdg.pred in
	    let cats = incats @ outcats in
	    let ts = List.map2 (fun x y -> (x, y)) cats jdg.args in 
	      pf ppf "%s(@[%a@])" jdg.pred
		(emit_comseq (fun ppf (cat, t) -> emit_term n tbl env cat ppf t)) ts
	  with Not_found -> failwith ("emit_jdg: " ^ jdg.pred ^ " not found")
	    | Invalid_argument("List.map2") -> failwith ("emit_jdg: arity mismatch for " ^ jdg.pred)

    let emit_pat_of_rule ppf rname =
      pf ppf 
	"@[<2>{@[conc = _conc_;@ by = \"%s\";@ since = _derivs_;@ pos = _p_@]}@]"
	rname

    let emit_pat_of_jdg i env ppf jdg = 
      let tbl = Hashtbl.create 50 in
	pf ppf "@[  %a@ " (emit_jdg i tbl env) jdg;
	if Hashtbl.fold (fun _ m res -> res || m > 1) tbl false then
	  pf ppf "when @[%a@]" (emit_eqs i) tbl;
	pf ppf "@]";
	tbl

    let emit_pat_of_derivs ppf prems =
      let rec aux ppf i prem =
	match prem with
	    J _ -> pf ppf "_d%d_;@ " i
	  | Qexp _ -> ()
      in pf ppf "@[@ @ [@[%a@]]@]" (fun ppf -> iteri (aux ppf)) prems

    let merge_tables master local i =
      let aux id n = (* for each item (id, n) in local *)
	if n > 0 then  (* if id appears, then add the index i to the master*)
	  let old = try Hashtbl.find master id with Not_found -> [] in
	    Hashtbl.replace master id (i::old)
      in
	Hashtbl.iter aux local

    let rec emit_exp_of_premises i rn tbl env ppf = function
	[] -> 
	  begin 
	    pf ppf "@[if @[%a@]@ then _conc_" emit_eqs' tbl; 
	    pf ppf "@ else errAt _p_ \"Wrong rule application: %s\"@]" rn
	  end
      | J prem :: rest ->
	  pf ppf "@[(";
	  begin 
	    pf ppf "@[<v>@[match check_deriv _d%d_ with@]@ " i;
	    pf ppf "@[<4>";
	    (* pat -> *) 
	    pf ppf "@[  ";
	    merge_tables tbl (emit_pat_of_jdg i env ppf prem) i;
	    pf ppf " ->@]@ %a@]@ "
	      (* exp *) (emit_exp_of_premises (i+1) rn tbl env) rest;
	    pf ppf "@[| _ -> errAt _p_ \"The form of the %a premise is wrong: %s\"@]@]" emit_ordinal i rn;
	  end;
	  pf ppf ")@]"
      | Qexp (s, _) :: rest ->
	  let b = Buffer.create (String.length s + 10) in
	  let freshvarp = ref false in
	  let subst s = 
	    try 
	      match Hashtbl.find tbl s with 
		  j :: _ -> (String.make j '_') ^ s
		| _ -> raise Not_found
	    with Not_found -> 
	      (* found a fresh variable, which is assumed to be on lhs *)
	      Hashtbl.replace tbl s [i];
	      freshvarp := true; 
	      (String.make i '_') ^ s
		(*	    failwith ("emit_exp_of_premises: " ^
			    s ^ " doesn't appear in preceding premises")
		*)
	  in
	    add_substitute b subst s;
	    if !freshvarp then
	      pf ppf "@[let @[%s@]@ in@ %a@]" 
		(Buffer.contents b)
		(emit_exp_of_premises (i+1) rn tbl env) rest
	    else
	      pf ppf 
		"@[if @[%s@]@ then %a@ else errAt _p_ \"Wrong rule application: %s\"@]" 
		(Buffer.contents b)
		(emit_exp_of_premises (i+1) rn tbl env) rest
		rn

    let emit_clause_of_rule env ppf r =
      pf ppf "| @[<4>%a ->@ " emit_pat_of_rule r.rname;
      pf ppf "@[(";
      begin
	pf ppf "@[<v>@[match _conc_ with@]@ ";
	pf ppf "@[<2>"; 
	let tbl = Hashtbl.create 50 in  (* for recording variable occurrences *)
          merge_tables tbl (emit_pat_of_jdg 0 env ppf r.rconc) 0; 
          pf ppf " ->@ ";
          pf ppf "@[(";
          begin
  	    pf ppf "@[<v>@[match _derivs_ with@]@ ";
	    pf ppf "@[<2>%a ->@ %a@]@ "
	      emit_pat_of_derivs r.rprem
	      (emit_exp_of_premises 1 r.rname tbl env) r.rprem;
	    pf ppf "@[| _ -> errAt _p_ \"The number of premises is wrong: %s\"@]@]" r.rname;
	  end;
	  pf ppf ")@]@]@ ";
	  pf ppf "@[| _ -> errAt _p_ \"The form of conclusion is wrong: %s\"@]@]" r.rname
      end;
      pf ppf ")@]@]"

    let emit env ppf rules = 
      let rec loop ppf = function
	  [] -> pf ppf "@[| {by=_name_; pos=_p_} -> errAt _p_ (\"No such rule: \" ^ _name_)@]"
	| rule::rest ->
	    pf ppf "%a@ %a" (emit_clause_of_rule env) rule loop rest
      in
	pf ppf "@[<v>@[let rec check_deriv = function@]@ %a@]" loop rules;
  end

  let of_rules = Rules.emit
end

module type SIMPLE_EMITTER =
sig
  val of_bnf : Format.formatter -> string -> syndef list -> unit
  val of_judgments : Format.formatter -> string -> (judgment * int) list -> unit 
    (* integers specifies how many arguments are considered as input *)
  val of_rules : Format.formatter -> string -> rule list -> unit
end

module TeX =
struct
  let rec emit_term gn ppf t = match t with
      Var ("true" | "false" as b) -> pf ppf "\\texttt{%s}" b
    | Var x -> 
	let (base, suffix, primes) = Syntax.split_LCID x in
	  if suffix = "" then
	    pf ppf "\\%smv{%s}{%s}" gn base primes
	  else pf ppf "\\%smv{%s}{%s_{%s}}" gn base primes suffix
    | App (f, []) -> pf ppf "\\%s%sTerm" gn f
    | App (f, ts) -> pf ppf "\\%s%sTerm@[{%a}@]" gn f (emit_terms gn) ts
  and emit_terms gn ppf = function
      [] -> ()
    | t :: [] -> emit_term gn ppf t
    | t :: rest -> pf ppf "%a}@]@,@[{%a" (emit_term gn) t (emit_terms gn) rest

  let emit_qexp gn ppf s =
    let b = Buffer.create 30 in
      add_substitute b 
	(fun s -> 
	   let (base, suffix, primes) = Syntax.split_LCID s in
	     if suffix = "" then
	       "\\" ^ gn ^ "mv{" ^ base ^ "}{" ^ primes ^ "}" 
	     else "\\" ^ gn ^ "mv{" ^ base ^ "}{" ^ primes ^ "_{" ^ suffix ^ "}}")
	s;
      pf ppf "(%s)" (Buffer.contents b)

  let emit_bnf gn ppf sdef =
    emit_comseq (fun ppf s -> emit_term gn ppf (Var s)) ppf sdef.mvars;
    pf ppf "@ \\in@ \\mathbf{%s}" sdef.cat;
    match sdef.body with [] -> () | _ -> pf ppf "@ ::=@ ";
    emit_seq "\\mid" (emit_term gn) ppf sdef.body

  let emit_jdg gn ppf j = 
    pf ppf "\\%s%s@[<2>@[{%a}@]@]" gn j.pred (emit_terms gn) j.args

  let rec emit_premises gn ppf = function
      [] -> ()
    | J j :: [] -> emit_jdg gn ppf j
    | (Qexp (q, None) | Qexp (_, Some q)) :: [] -> emit_qexp gn ppf q
    | J j :: rest -> 
	pf ppf "@[%a@]@ \\andalso@ %a" (emit_jdg gn) j (emit_premises gn) rest
    | (Qexp (q, None) | Qexp (_, Some q)) :: rest -> 
	pf ppf "@[%a@]@ \\andalso@ %a" (emit_qexp gn) q (emit_premises gn) rest

  let normalize_name s =
    let roman = function
	'1' -> "i" | '2' -> "ii" | '3' -> "iii" | '4' -> "iv" | '5' -> "v"
      | '6' -> "vi" | '7' -> "vii" | '8' -> "viii" | '9' -> "ix" in
    let b = Buffer.create (String.length s) in
      String.iter 
	(fun c -> 
	   if ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') then
	     Buffer.add_char b c 
	   else if ('1' <= c && c <= '9') then 
	     Buffer.add_string b (roman c)) 
	s; 
      Buffer.contents b

  let emit gn ppf r =
    let gn = normalize_name gn in
      pf ppf "@[\\long\\def\\%s%s{\\infrule[%s]{@;<0 2>@[<v>%a@]@;<0 0>}{@;<0 2>@[<v>%a@]}}@]@ " 
	gn
	(normalize_name r.rname)
	r.rname 
	(emit_premises gn) r.rprem
	(emit_jdg gn) r.rconc
	
  let of_bnf ppf gname syndefs =
    pf ppf "\\long\\def\\%sDisplayBNF{@,\\[\\begin{array}{l}@\n"
      (normalize_name gname);
    List.iter 
      (fun sd -> 
	 emit_bnf (normalize_name gname) ppf sd;
	 pf ppf "\\\\@\n") 
      syndefs;
    pf ppf "\\end{array}\\]@,}@."

  let of_judgments _ = failwith "Emit.TeX.of_judgments not implemented"

  let of_rules ppf gname rules = 
    let gname = normalize_name gname in
      List.iter
	(fun r -> 
	   pf ppf "@[<v>%a@]@." (emit gname) r) 
	rules;
      pf ppf "\\def\\%sDisplayRules{@," gname;
      List.iter 
	(fun r ->
	   pf ppf "\\%s%s@," gname (normalize_name r.rname))
	rules;
      pf ppf "}"
end

module SExp =
struct
  let rec emit_term gn ppf t = match t with
      Var ("true" | "false" as b) -> pf ppf "\"\\\\mbox{%s}\"" b
    | Var x -> 
	let (base, suffix, primes) = Syntax.split_LCID x in
	  if suffix = "" then
	    pf ppf "(%s:mv \"%s%s\")" gn base primes
	  else pf ppf "(%s:mv \"%s%s\" \"%s\")" gn base primes suffix
    | App (f, ts) -> pf ppf "(%s:%sTerm@[%a@])" gn f (emit_terms gn) ts
  and emit_terms gn ppf = function
      [] -> ()
    | t :: [] -> pf ppf "@ %a" (emit_term gn) t
    | t :: rest -> pf ppf "@ %a@]@[%a" (emit_term gn) t (emit_terms gn) rest

  let emit_qexp gn ppf s =
    let b = Buffer.create 40 in
      add_substitute b 
	(fun s -> 
	   let (base, suffix, primes) = Syntax.split_LCID s in
	     if suffix = "" then ",("^ gn ^":mv \\\"" ^ base ^ primes ^ "\\\")"
	     else ",("^ gn ^":mv \\\"" ^ base ^ primes ^ "\\\" \\\"" ^ suffix ^ "\\\")")
	(escaped_for_Scheme s);
      pf ppf "#`\"(%s)\"" (Buffer.contents b)

  let emit_bnf gn ppf sdef =
    pf ppf "(bnf \"%s\" (list %a) (list %a))" 
      sdef.cat
      (emit_seq "" (fun ppf s -> emit_term gn ppf (Var s))) sdef.mvars
      (emit_seq "" (emit_term gn)) sdef.body

  let emit_jdg gn ppf j = 
    pf ppf "(%s:%s@[<2>@[%a@]@])" gn j.pred (emit_terms gn) j.args

  let rec emit_premises gn ppf = function
      [] -> ()
    | J j :: [] -> pf ppf "@ @[%a@]" (emit_jdg gn) j
    | (Qexp (q, None) | Qexp (_, Some q)) :: [] -> 
	pf ppf "@ @[%a@]" (emit_qexp gn) q
    | J j :: rest -> 
	pf ppf "@ @[%a@]%a" (emit_jdg gn) j (emit_premises gn) rest
    | (Qexp (q, None) | Qexp (_, Some q)) :: rest -> 
	pf ppf "@ @[%a@]%a" (emit_qexp gn) q (emit_premises gn) rest
	  
  let emit gn ppf r =
    pf ppf "(@[<v 1>infrule@ \"%s\"@;(@[<v 1>list%a@])@;@[<v>%a@]@])" 
      r.rname 
      (emit_premises gn) r.rprem
      (emit_jdg gn) r.rconc
      

  let of_bnf ppf gname syndefs =
    pf ppf "(@[<v 1>define bnfdefs@ (@[<v 1>list";
    List.iter (fun sd -> 
		 pf ppf "@ @[<v>%a@]" (emit_bnf gname) sd) 
      syndefs;
    pf ppf "@])@])@.";
    pp_print_newline ppf ()

  let of_judgments _ = failwith "Emit.SExp.of_judgments not implemented"

  let of_rules ppf gname rules = 
    pf ppf "(@[<v 1>define rulenames@ (@[<v 1>list";
    List.iter (fun r ->
		 pf ppf "@ \"%s\"" r.rname) rules;
    pf ppf "@])@])@.";
    pp_print_newline ppf ();
    pf ppf "(@[<v 1>define rules@ (@[<v 1>list";
    List.iter (fun r -> 
		 pf ppf "@ @[<v>%a@]" (emit gname) r) 
      rules;
    pf ppf "@])@])@."
end


module Prover =
struct

  let pf = fprintf

  let of_bnf _ = failwith "Prover.of_bnf not implemented"

  (* emit type definition from judgment definitions *)
  let of_judgments env ppf jdgs =
    let emit_jdg env ppf = function
	({pred = pred; args = []}, _) -> pf ppf "In_%s" pred
      | (jdg, i) ->
	  let inargs = take i jdg.args in
	  let ts = 
	    List.map 
	      (fun (Var v) -> Var (Syntax.base_LCID v)) inargs in
	    pf ppf "In_%s of @[%a@]" 
	      jdg.pred
	      (emit_seq "*" (emit_var env)) ts
    in
    pf ppf "@[type in_judgment @[= %a@]@]"
      (emit_barseq (emit_jdg env)) jdgs

  (* emit a term as an expression.  Doesn't have to care about
     multiple occurrences of the same variable. *)
  let emit_term env cat ppf term = 
    let rec aux  ppf (cat, term) = match term with
	Var id -> 
	  let cat' = 
	    try Syntax.Env.lookup_cat env (Syntax.base_LCID id) with
		Not_found -> failwith ("emit_term: " ^ id ^ " not found") 
	  in
	    pf ppf "%a%s" (emit_coercion env) (cat', cat) id
      | App (id, ts) -> 
	  let (cats, cat') = 
	    try Syntax.Env.lookup_tcon env id with
		Not_found -> failwith ("emit_term: " ^ id ^ " not found")
	  in
	    match ts with 
		[] -> pf ppf "%a%s" (emit_coercion env) (cat', cat) id
	      | _ -> 
		  let ts = List.map2 (fun x y -> (x, y)) cats ts in 
		    pf ppf "%a(%s(@[%a@]))" 
		      (emit_coercion env) (cat', cat)
		      id 
		      (emit_comseq aux) ts
    in
      aux ppf (cat, term)

  let emit_pat_of_jdg_in tbl env ppf jdg = 
    match jdg with
	{pred = pred; args = []} -> pf ppf "In_%s" pred
      | jdg ->
	  try 
	    let (incats, _) = Syntax.Env.lookup_jcon env jdg.pred in
	    let inargs = take (List.length incats) jdg.args in
	    let ts = List.map2 (fun x y -> (x, y)) incats inargs in 
	      pf ppf "In_%s(@[%a@])" jdg.pred
		(emit_comseq (fun ppf (cat, t) -> ML.Rules.emit_term 0 tbl env cat ppf t)) ts
	  with Not_found -> failwith ("emit_pat_of_jdg_in: " ^ jdg.pred ^ " not found")

  let emit_exp_of_jdg_in env ppf = function
      {pred = pred; args = []} -> pf ppf "In_%s" pred
    | jdg ->
	try 
	  let (incats, _) = Syntax.Env.lookup_jcon env jdg.pred in
	  let inargs = take (List.length incats) jdg.args in
	  let ts = List.map2 (fun x y -> (x, y)) incats inargs in 
	    pf ppf "In_%s(@[%a@])" jdg.pred
	      (emit_comseq (fun ppf (cat, t) -> emit_term env cat ppf t)) ts
	with Not_found -> failwith ("emit_exp_of_jdg_in: " ^ jdg.pred ^ " not found")

  let emit_pat_of_jdg_out tbl env ppf = function
      {pred = pred; args = []} -> pp_print_string ppf pred
    | jdg ->
	try 
	  let (incats, outcats) = Syntax.Env.lookup_jcon env jdg.pred in
	  let outargs = drop (List.length incats) jdg.args in
	  let ts = List.map2 (fun x y -> (x, y)) outcats outargs in 
	    pf ppf "%s(@[%a" jdg.pred
	      (emit_comseq (fun ppf _ -> pp_print_string ppf "_")) incats;
	    (match ts with [] -> pf ppf "@])"
	       | _ -> 
		   pf ppf ",@ %a@])"
		     (emit_comseq (fun ppf (cat, t) -> ML.Rules.emit_term 0 tbl env cat ppf t)) ts)
	with Not_found -> failwith ("emit_pat_of_jdg_out: " ^ jdg.pred ^ " not found")

  let emit_jdg env ppf = function
      {pred = pred; args = []} -> pp_print_string ppf pred
    | jdg ->
	try 
	  let (incats, outcats) = Syntax.Env.lookup_jcon env jdg.pred in
	  let ts = List.map2 (fun x y -> (x, y)) (incats @ outcats) jdg.args in 
	    pf ppf "%s(@[%a@])" jdg.pred
	      (emit_comseq (fun ppf (cat, t) -> emit_term env cat ppf t)) ts
	with Not_found -> failwith ("emit_jdg: " ^ jdg.pred ^ " not found")

  let emit_exp_of_premises tbl env ppf r = 
    let rec aux i ppf = function
	[] -> 
	  if Hashtbl.fold (fun _ m res -> res || m > 1) tbl false then
	    begin
	      pf ppf "@[%a@]@ || " (ML.Rules.emit_eqs 0) tbl;
	      pf ppf 
		"@[(for j = 1 to %d do ignore (Stack.pop deriv_stack) done; false)@]" 
		(i-1)
	    end
	  else pf ppf "@ true@ "
      | J prem :: rest ->
	  begin
	    pf ppf 
	      "@[let _d%d_ = make_deriv (%a) in@]@ " i (emit_exp_of_jdg_in env) prem;
	    pf ppf "@[Stack.push _d%d_ deriv_stack;@]@ " i;
	    pf ppf 
	      "@[<v2>@[(match _d%d_.conc with@]@ @[<v3>  %a ->@ @[%a@]@]@ " 
	      i
	      (emit_pat_of_jdg_out tbl env) prem
	      (aux (i+1)) rest;
	    pf ppf "@[| _ -> for j = 1 to %d do ignore (Stack.pop deriv_stack) done; false@]" i;
	    pf ppf "@,)@]"
	  end
      | Qexp (s, _) :: rest ->
	  let b = Buffer.create (String.length s + 10) in
	  let freshvarp = ref false in
	  let subst s =
 	    (* checks if s is a fresh variable, which is supposed
	       to be lhs of equality *)
	  freshvarp := !freshvarp || not (Hashtbl.mem tbl s);
	  s 
	in
	  add_substitute b subst s;
	  if !freshvarp then 
	    pf ppf "@[let @[%s@]@ in @]@ %a"
	      (Buffer.contents b) (aux i) rest
	  else pf ppf "if (@[%s@]) then@ %a else (for j = 1 to %d do ignore (Stack.pop deriv_stack) done; false)" (Buffer.contents b) (aux i) rest (i - 1)
    in aux 1 ppf r.rprem

  let emit_clause_of_rule env ppf r =
    let tbl = Hashtbl.create 50 in
    pf ppf "| @[<4>%a when %a"
      (emit_pat_of_jdg_in tbl env) r.rconc
      (ML.Rules.emit_eqs 0) tbl;
    (* creating a backup copy for later pattern generation *)
    let tbl' = Hashtbl.copy tbl in 
    pf ppf " && @\n%a -> @\n" (emit_exp_of_premises tbl env) r;
    begin 
      (* extract relevant parts from subderivations 
	 and construct the conclusion *)
      pf ppf "@[let _subderivs_ = pop %d deriv_stack [] in@]@ " 
	(List.fold_right
	   (fun x y -> match x with J j -> 1 + y | _ -> y) r.rprem 0);
      pf ppf "@[<v2>@[(match List.map (fun d -> d.conc) _subderivs_ with@]@ ";
      pf ppf "@[[%a]@]"
	(emit_semiseq (emit_pat_of_jdg_out tbl' env))
	(List.fold_right
	   (fun x y -> match x with J j -> j :: y | _ -> y) r.rprem []);
      (* Actually, the following equations don't have to be
	 generated since they have already been checked *)
      if Hashtbl.fold (fun _ m res -> res || m > 1) tbl' false then
	pf ppf "@ when @[%a@] ->@ " (ML.Rules.emit_eqs 0) tbl'
      else pf ppf " ->@ ";
      List.iter
	(function 
	     J _ -> () 
	   | Qexp (s, _) ->
	       let b = Buffer.create (String.length s + 10) in
	       let freshvarp = ref false in
	       let subst s = 
		 (* checks if s is a fresh variable, which is supposed
		    to be lhs of equality *)
		 freshvarp := !freshvarp || not (Hashtbl.mem tbl' s);
		 s in
		 add_substitute b subst s;
		 if !freshvarp then
		   pf ppf "@[let @[%s@]@ in @]@ " (Buffer.contents b)
		 else (* Actually, this check should be omitted since
			 the condition has already must have been satisfied *)
		   pf ppf 
		     "@[if not (@[%s@]) then err \"Implementation error!\" else@]@ "
		     (Buffer.contents b)
	)
	r.rprem;
      pf ppf "@[let _conc_ = %a in @]@ " (emit_jdg env) r.rconc;
      pf ppf "@[{@[conc = _conc_;@ by = \"%s\";@ since = _subderivs_;@ pos = dummy@]}@])@]@]" 
	r.rname
    end;
    pf ppf "@]"

  let of_rules env ppf rules = 
    let rec loop ppf = function
	[] -> pf ppf "@[| j -> raise (NoApplicableRule j)@]@\n" 
	  (* need to augment error information *)
      | rule::rest ->
	  pf ppf "%a@ %a"
	    (emit_clause_of_rule env) rule
	      loop rest
    in
      pf ppf "@[<v>@[let rec make_deriv = function@]@ %a@]" loop rules;
end
