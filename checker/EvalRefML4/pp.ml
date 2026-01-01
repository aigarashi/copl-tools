open Format
open Core

let g = "EvalRefMLiv"

let pr = fprintf

let is_negative i = (i < 0)

(* generic functions to generate parens depending on precedence *)
let with_paren lt ppf_e e_up ppf e =
  let (<) = lt in
  if e < e_up then pr ppf "(%a)" ppf_e e else pr ppf "%a" ppf_e e

(* precedence for expressions *)
let rec is_last_longexp = function
    BinOp(_,_,e) | Cons(_,e) -> is_last_longexp e
  | If(_,_,_) | Let(_,_,_) | Abs(_,_) | LetRec(_,_,_,_) | Match(_,_,_,_,_) -> true
  | _ -> false

(* if e is the left operand of e_up, do you need parentheses for e? *)
let (<) e e_up = match e, e_up with
    (* mult associates stronger than plus or minus *)
    BinOp((Plus | Minus | Lt), _, _), BinOp(Mult, _, _)
  | Cons(_,_),                        BinOp(Mult, _, _)
  | BinOp(Lt, _, _),                  BinOp((Plus | Minus), _, _)
  | Cons(_, _),                       BinOp((Plus | Minus), _, _)
  | BinOp(_, _, _),                   App(_, _)
  | Cons(_, _),                       App(_, _)
  | BinOp(Lt, _, _),                  Cons(_,_)
  | Cons(_,_),                        Cons(_, _)
      -> true
  | e,                                (BinOp(_,_,_) | App(_, _) | Cons(_, _)) when is_last_longexp e
      -> true
  | Exp_of_int i,                     App(_, _) when is_negative i -> true
  | _ -> false

(* if e is the right operand of e_up, do you need parentheses for e? *)
let (>) e_up e = match e_up, e with
    (* mult associates stronger than plus or minus,
       bin ops are left-associative, and
       function application, which is left-associative, is the strongest *)
  | App(_, _),                   App(_, _)
  | App(_, _),                   BinOp(_, _, _)
  | App(_, _),                   If(_, _, _)
  | App(_, _),                   Let(_, _, _)
  | App(_, _),                   Abs(_, _)
  | App(_, _),                   LetRec(_, _, _, _)
  | App(_, _),                   Match(_, _, _, _, _)
  | App(_, _),                   Cons(_, _)
  | BinOp(Mult, _, _),           (BinOp(_, _, _) | Cons(_, _))
  | BinOp((Plus | Minus), _, _), (BinOp((Plus | Minus | Lt), _, _) | Cons(_, _))
  | Cons(_, _),                  BinOp(Lt, _, _)
      -> true
  | App(_, _),                   Exp_of_int i when is_negative i -> true
  | _ -> false

let rec print_exp ppf e =
    let with_paren_L = with_paren (<)
    and with_paren_R = with_paren (fun e_up e -> e > e_up) in
      match e with
	  Exp_of_int i -> pr ppf "%d" i
	| Exp_of_bool true -> pr ppf "true"
	| Exp_of_bool false -> pr ppf "false"
	| Exp_of_Var (Var id) -> pp_print_string ppf id
	| BinOp(p, e1, e2) ->
	    let op =
	      match p with Plus -> "+" | Minus -> "-" | Mult -> "*" | Lt -> "<" in
	      pr ppf "%a %s %a"
		(with_paren_L print_exp e) e1
		op
		(with_paren_R print_exp e) e2
	| If(e1, e2, e3) ->
	    pr ppf "if %a then %a else %a"
	      print_exp e1
	      print_exp e2
	      print_exp e3
	| Let(Var x, e1, e2) ->
	    pr ppf "let %s = %a in %a"
	      x
	      print_exp e1
	      print_exp e2
	| Abs(Var x, e) ->
	    pr ppf "fun %s -> %a" x print_exp e
	| App(e1, e2) ->
	    pr ppf "%a %a"
	      (with_paren_L print_exp e) e1
	      (with_paren_R print_exp e) e2
	| LetRec(Var x, Var y, e1, e2) ->
	    pr ppf "let rec %s = fun %s -> %a in %a" x y
	      print_exp e1
	      print_exp e2
	| Nil -> pr ppf "[]"
	| Cons(e1, e2) ->
	    pr ppf "%a :: %a"
	      (with_paren_L print_exp e) e1
	      (with_paren_R print_exp e) e2
	| Match(e1, e2, Var x, Var y, e3) ->
	    pr ppf "match %a with [] -> %a | %s :: %s -> %a"
	      print_exp e1
	      print_exp e2
	      x
	      y
	      print_exp e3
	| NewRef e0 ->
	    pr ppf "ref %a"
	      (with_paren_L print_exp e) e0
	| Assign(e1, e2) ->
	    pr ppf "%a := %a"
	      (with_paren_L print_exp e) e1
	      (with_paren_R print_exp e) e2
	| Deref e0 ->
	    pr ppf "! %a" (with_paren_L print_exp e) e0

let rec print_env ppf = function
    Empty -> ()
  | Bind(env', Var x, v) -> pr ppf "%a%s = %a" print_env' env' x print_val v
and print_env' ppf = function
  | Empty -> ()
  | Bind(env', Var x, v) -> pr ppf "%a%s = %a,@ " print_env' env' x print_val v

and print_store ppf = function
    EmptyS -> ()
  | Block(s', Loc x, v) -> pr ppf "%a@@%s = %a" print_store' s' x print_val v
and print_store' ppf = function
  | EmptyS -> ()
  | Block(s', Loc x, v) -> pr ppf "%a@@%s = %a,@ " print_store' s' x print_val v

and print_val ppf v =
  let with_paren_L = with_paren (fun v v_up ->
				   match v, v_up with
				     | ConsV(_,_), ConsV(_,_) -> true
				     | _ -> false)
  and with_paren_R = with_paren (fun v_up v -> false) in
    match v with
	Value_of_int i -> pr ppf "%d" i
      | Value_of_bool true -> pr ppf "true"
      | Value_of_bool false -> pr ppf "false"
      | Value_of_Loc (Loc x) -> pr ppf "@@%s" x
      | Fun(env, Var x, e) -> pr ppf "(%a)[fun %s -> %a]" print_env env x print_exp e
      | Rec(env, Var x, Var y, e) -> pr ppf "(%a)[rec %s = fun %s -> %a]" print_env env x y print_exp e
      | NilV -> pr ppf "[]"
      | ConsV(v1,v2) ->
	  pr ppf "%a :: %a"
	    (with_paren_L print_val v) v1
	    (with_paren_R print_val v) v2

let print_judgment ppf = function
    EvalTo (EmptyS, env, e, v, EmptyS) ->
      pr ppf "@[@[%a@]@ |- @[%a@] evalto %a@]" print_env env print_exp e print_val v
  | EvalTo (EmptyS, env, e, v, s2) ->
      pr ppf "@[@[%a@]@ |- @[%a@] evalto @[%a /@ @[%a@]@]@]" print_env env print_exp e print_val v print_store s2
  | EvalTo (s1, env, e, v, EmptyS) ->
      pr ppf "@[@[%a@]/@ @[%a@]@ |- @[%a@] evalto %a@]" print_store s1 print_env env print_exp e print_val v
  | EvalTo (s1, env, e, v, s2) ->
      pr ppf "@[@[%a@]/@ @[%a@]@ |- @[%a@] evalto @[%a /@ @[%a@]@]@]" print_store s1 print_env env print_exp e print_val v print_store s2
  | AppBOp (Lt, v1, v2, Value_of_bool true) ->
      pr ppf "@[%a is less than %a@]" print_val v1 print_val v2
  | AppBOp (Lt, v1, v2, Value_of_bool false) ->
      pr ppf "@[%a is not less than %a@]" print_val v1 print_val v2
  | AppBOp (p, v1, v2, v3) ->
      let op = match p with Plus -> "plus" | Minus -> "minus" | Mult -> "times"
      in pr ppf "@[%a %s %a is %a@]" print_val v1 op print_val v2 print_val v3

let print_pjudgment ppf = function
    In_EvalTo (EmptyS, env, e) ->
      pr ppf "@[%a@]@ |- %a evalto ?" print_env env print_exp e
  | In_EvalTo (s, env, e) ->
      pr ppf "@[%a@]/@ @[%a@]@ |- %a evalto ?" print_store s print_env env print_exp e
  | In_AppBOp (Lt, v1, v2) ->
      pr ppf "%a is less than %a ?" print_val v1 print_val v2
  | In_AppBOp (p, v1, v2) ->
      let op = match p with Plus -> "plus" | Minus -> "minus" | Mult -> "times"
      in pr ppf "%a %s %a is ?" print_val v1 op print_val v2

let rec tex_exp ppf e =
    let with_paren_L = with_paren (<)
    and with_paren_R = with_paren (fun e_up e -> e > e_up) in
      match e with
	  Exp_of_int i -> pr ppf "%d" i
	| Exp_of_bool b -> pp_print_string ppf (string_of_bool b)
	| Exp_of_Var (Var id) -> pp_print_string ppf id
	| BinOp(p, e1, e2) ->
	    let op =
	      match p with Plus -> "+" | Minus -> "-" | Mult -> "*" | Lt -> "<" in
	      pr ppf "\\%sBinOpTerm{%a}{%s}{%a}" g
		(with_paren_L tex_exp e) e1
		op
		(with_paren_R tex_exp e) e2
	| If(e1, e2, e3) ->
	    pr ppf "\\%sIfTerm{%a}{%a}{%a}" g
	      tex_exp e1
	      tex_exp e2
	      tex_exp e3
	| Let(Var x, e1, e2) ->
	    pr ppf "\\%sLetTerm{%s}{%a}{%a}" g
	      x
	      tex_exp e1
	      tex_exp e2
	| Abs(Var x, e) ->
	    pr ppf "\\%sFunTerm{%s}{%a}" g x tex_exp e
	| App(e1, e2) ->
	    pr ppf "\\%sAppTerm{%a}{%a}" g
	      (with_paren_L tex_exp e) e1
	      (with_paren_R tex_exp e) e2
	| LetRec(Var x, Var y, e1, e2) ->
	    pr ppf "\\%sLetRecTerm{%s}{%s}{%a}{%a}" g
	      x y tex_exp e1 tex_exp e2
	| Nil -> pr ppf "\\%sNilTerm" g
	| Cons(e1, e2) ->
	    pr ppf "\\%sConsTerm{%a}{%a}" g
	      (with_paren_L tex_exp e) e1
	      (with_paren_R tex_exp e) e2
	| Match(e1, e2, Var x, Var y, e3) ->
	    pr ppf "\\%sMatchTerm{%a}{%a}{%s}{%s}{%a}" g
	      tex_exp e1
	      tex_exp e2
	      x
	      y
	      tex_exp e3
	| NewRef e0 ->
	    pr ppf "\\%sRefTerm{%a}" g
	      tex_exp e0
	| Assign(e1, e2) ->
	    pr ppf "\\%sAssignTerm{%a}{%a}" g
	      tex_exp e1  tex_exp e2
	| Deref e0 ->
	    pr ppf "\\%sDerefTerm{%a}" g
	      tex_exp e0

let rec tex_env ppf = function
    Empty -> ()
  | Bind(env', Var x, v) -> pr ppf "%a%s = %a" tex_env' env' x tex_val v
and tex_env' ppf = function
  | Empty -> ()
  | Bind(env', Var x, v) -> pr ppf "%a%s = %a,@ " tex_env' env' x tex_val v

and tex_store ppf = function
    EmptyS-> ()
  | Block(s', Loc x, v) -> pr ppf "%a@@%s = %a" tex_store' s' x tex_val v
and tex_store' ppf = function
  | EmptyS -> ()
  | Block(s', Loc x, v) -> pr ppf "%a@@%s = %a,@ " tex_store' s' x tex_val v

and tex_val ppf  v =
  let with_paren_L = with_paren (fun v v_up ->
				   match v, v_up with
				     | ConsV(_,_), ConsV(_,_) -> true
				     | _ -> false)
  and with_paren_R = with_paren (fun v_up v -> false) in
    match v with
	Value_of_int i -> pr ppf "%d" i
      | Value_of_bool b -> pp_print_string ppf (string_of_bool b)
      | Value_of_Loc (Loc x) -> pr ppf "@@%s" x
      | Fun(env, Var x, e) -> pr ppf "\\%sFunTerm{%a}{%s}{%a}" g tex_env env x tex_exp e
      | Rec(env, Var x, Var y, e) -> pr ppf "\\%sRecTerm{%a}{%s}{%s}{%a}" g
	  tex_env env x y tex_exp e
      | NilV -> pr ppf "\\%sNilVTerm" g
      | ConsV(v1,v2) ->
	  pr ppf "\\%sConsVTerm{%a}{%a}" g
	    (with_paren_L tex_val v) v1
	    (with_paren_R tex_val v) v2

let tex_judgment ppf = function
    EvalTo (s1, env, e, v, s2) ->
      pr ppf "\\%sEvalTo{%a}{%a}{%a}{%a}{%a}" g
	tex_store s1  tex_env env  tex_exp e  tex_val v  tex_store s2
  | AppBOp (p, v1, v2, v3) ->
      let op = match p with
	  Plus -> "\\MLivPlusTerm" | Minus -> "\\MLivMinusTerm"
	| Mult -> "\\MLivMultTerm" | Lt -> "\\MLivLTTerm"
      in pr ppf "\\%sAppBOp{%a}{%s}{%a}{%a}" g
	   tex_val v1 op tex_val v2 tex_val v3
