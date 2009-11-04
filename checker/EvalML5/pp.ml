open Format
open Core

let g = "MLv"

let pr = fprintf

let is_negative i = (i < 0)

(* generic functions to generate parens depending on precedence *)
let with_paren lt ppf_e e_up ppf e =
  let (<) = lt in
  if e < e_up then pr ppf "(%a)" ppf_e e else pr ppf "%a" ppf_e e

(* precedence for patterns *)
(* if p is the left operand of p_up, do you need parentheses for p? *)
let (<) p p_up = match p, p_up with
  | ConsP(_,_), ConsP(_,_) -> true
  | _ -> false

(* if p is the right operand of p_up, do you need parentheses for p? *)
let (>) p_up p = false

let rec print_pat ppf p = 
  let with_paren_L = with_paren (<) 
  and with_paren_R = with_paren (fun p_up p -> p > p_up) in
    match p with
	Pat_of_Var (Var x) -> pr ppf "%s" x
      | WildP -> pr ppf "_"
      | NilP -> pr ppf "[]"
      | ConsP(p1,p2) -> 
	  pr ppf "%a :: %a" 
	    (with_paren_L print_pat p) p1 
	    (with_paren_R print_pat p) p2

let rec tex_pat ppf p = 
  let with_paren_L = with_paren (<) 
  and with_paren_R = with_paren (fun p_up p -> p > p_up) in
    match p with
	Pat_of_Var (Var x) -> pr ppf "%s" x
      | WildP -> pr ppf "\\%sWildPTerm" g
      | NilP -> pr ppf "\\%sNilPTerm" g
      | ConsP(p1,p2) -> 
	  pr ppf "\\%sConsPTerm{%a}{%a}" g
	    (with_paren_L tex_pat p) p1 
	    (with_paren_R tex_pat p) p2

(* precedence for expressions *)
(* if e is the left operand of e_up, do you need parentheses for e? *)
let (<) e e_up = match e, e_up with
    (* mult associates stronger than plus, minus, lt, and cons
       plus and minus associates stronger than cons and lt
       cons associates to the right and stronger than lt *)
    BinOp((Plus | Minus | Lt), _, _), BinOp(Mult, _, _) 
  | Cons(_,_),                        BinOp(Mult, _, _)
  | BinOp(Lt, _, _),                  BinOp((Plus | Minus), _, _)
  | Cons(_, _),                       BinOp((Plus | Minus), _, _)
  | If(_, _, _),                      BinOp(_, _, _)
  | Let(_, _, _),                     BinOp(_, _, _)
  | Abs(_, _),                        BinOp(_, _, _)
  | LetRec(_, _, _, _),               BinOp(_, _, _)
  | Match(_, _),                      BinOp(_, _, _)
  | BinOp(_, _, _),                   App(_, _)
  | If(_, _, _),                      App(_, _)
  | Let(_, _, _),                     App(_, _)
  | Abs(_, _),                        App(_, _)
  | LetRec(_, _, _, _),               App(_, _)
  | Match(_, _),                      App(_, _)
  | Cons(_, _),                       App(_, _)
  | BinOp(Lt, _, _),                  Cons(_,_)
  | If(_, _, _),                      Cons(_, _)
  | Let(_, _, _),                     Cons(_, _)
  | Abs(_, _),                        Cons(_, _)
  | LetRec(_, _, _, _),               Cons(_, _)
  | Cons(_,_),                        Cons(_, _)
  | Match(_, _),                      Cons(_, _)
      -> true
  | Exp_of_int i,                     App(_, _) when is_negative i -> true
  | _ -> false

(* if e is the right operand of e_up, do you need parentheses for e? *)
let (>) e_up e = match e_up, e with
    (* mult associates stronger than plus, minus, and lt,
       plus and minus associates stronger than cons and lt,
       cons associates stronger than lt,
       bin ops are left-associative, and
       function application, which is left-associative, is the strongest *)
  | App(_, _),                   App(_, _)
  | App(_, _),                   BinOp(_, _, _)
  | App(_, _),                   If(_, _, _)
  | App(_, _),                   Let(_, _, _)
  | App(_, _),                   Abs(_, _)
  | App(_, _),                   LetRec(_, _, _, _)
  | App(_, _),                   Match(_, _)
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
      | Match(e, c) ->
	  pr ppf "match %a with %a"
	    print_exp e
	    print_clause c

and print_clause ppf c =
  let rec loop ppf c = 
    match c with 
	EmptyC -> ()
      | AddC(p, e, c') -> 
	  pr ppf " | %a -> %a%a" print_pat p print_exp e loop c'
  in
    match c with 
	EmptyC -> ()
      | AddC(p, e, c') -> 
	  pr ppf " %a -> %a%a" print_pat p print_exp e loop c'

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
      | Match(e, c) ->
	  pr ppf "\\%sMatchTerm{%a}{%a}" g
	    tex_exp e
	    tex_clause c

and tex_clause ppf = function
    EmptyC -> pr ppf "\\%sEmptyCTerm" g
  | AddC(p, e, c') -> 
      pr ppf "\\%sAddCTerm{%a}{%a}{%a}" g tex_pat p tex_exp e tex_clause c'

(* precedence for values *)
(* if v is the left operand of v_up, do you need parentheses for v? *)
let (<) v v_up = match v, v_up with
  | ConsV(_,_), ConsV(_,_) -> true
  | _ -> false

(* if v is the right operand of v_up, do you need parentheses for v? *)
let (>) v_up v = false

let rec print_env ppf = function
    Empty -> ()
  | Bind(env', Var x, v) -> pr ppf "%a%s = %a" print_env' env' x print_val v 
and print_env' ppf = function
  | Empty -> ()
  | Bind(env', Var x, v) -> pr ppf "%a%s = %a,@ " print_env' env' x print_val v 

and print_val ppf v = 
  let with_paren_L = with_paren (<) 
  and with_paren_R = with_paren (fun v_up v -> v > v_up) in
    match v with
	Value_of_int i -> pr ppf "%d" i
      | Value_of_bool true -> pr ppf "true"
      | Value_of_bool false -> pr ppf "false"
      | Fun(env, Var x, e) -> pr ppf "(%a)[fun %s -> %a]" print_env env x print_exp e
      | Rec(env, Var x, Var y, e) -> pr ppf "(%a)[rec %s = fun %s -> %a]" print_env env x y print_exp e
      | NilV -> pr ppf "[]"
      | ConsV(v1,v2) -> 
	  pr ppf "%a :: %a" 
	    (with_paren_L print_val v) v1 
	    (with_paren_R print_val v) v2

let rec tex_env ppf = function
    Empty -> ()
  | Bind(env', Var x, v) -> pr ppf "%a%s = %a" tex_env' env' x tex_val v 
and tex_env' ppf = function
  | Empty -> ()
  | Bind(env', Var x, v) -> pr ppf "%a%s = %a,@ " tex_env' env' x tex_val v 

and tex_val ppf v = 
  let with_paren_L = with_paren (<) 
  and with_paren_R = with_paren (fun v_up v -> v > v_up) in
    match v with
	Value_of_int i -> pr ppf "%d" i
      | Value_of_bool b -> pp_print_string ppf (string_of_bool b)
      | Fun(env, Var x, e) -> pr ppf "\\%sFunTerm{%a}{%s}{%a}" g tex_env env x tex_exp e
      | Rec(env, Var x, Var y, e) -> pr ppf "\\%sRecTerm{%a}{%s}{%s}{%a}" g 
	  tex_env env x y tex_exp e
      | NilV -> pr ppf "\\%sNilVTerm" g
      | ConsV(v1,v2) -> 
	  pr ppf "\\%sConsVTerm{%a}{%a}" g
	    (with_paren_L tex_val v) v1 
	    (with_paren_R tex_val v) v2

let print_judgment ppf = function
    EvalTo (env, e, v) -> 
      pr ppf "@[@[%a@]@ |- @[%a@] evalto %a@]" print_env env print_exp e print_val v
  | Matches (v, p, Res_of_Env env) ->
      pr ppf "@[@[%a@] matches @[%a@] when @[(%a)@]@]" print_val v print_pat p print_env env
  | Matches (v, p, Fail) ->
      pr ppf "@[@[%a@] doesn't match @[%a@]@]" print_val v print_pat p
  | AppBOp (Lt, v1, v2, Value_of_bool true) ->
      pr ppf "@[%a is less than %a@]" print_val v1 print_val v2
  | AppBOp (Lt, v1, v2, Value_of_bool false) ->
      pr ppf "@[%a is not less than %a@]" print_val v1 print_val v2
  | AppBOp (p, v1, v2, v3) -> 
      let op = match p with Plus -> "plus" | Minus -> "minus" | Mult -> "times"
      in pr ppf "@[%a %s %a is %a@]" print_val v1 op print_val v2 print_val v3

let print_pjudgment ppf = function
    In_EvalTo (env, e) ->
      pr ppf "@[%a@]@ |- %a evalto ?" print_env env print_exp e 
  | In_Matches (v, p) ->
      pr ppf "@[@[%a@] matches @[%a@] when ?@]" print_val v print_pat p
  | In_AppBOp (Lt, v1, v2) ->
      pr ppf "%a is less than %a ?" print_val v1 print_val v2
  | In_AppBOp (p, v1, v2) -> 
      let op = match p with Plus -> "plus" | Minus -> "minus" | Mult -> "times"
      in pr ppf "%a %s %a is ?" print_val v1 op print_val v2

let tex_res ppf = function
    Res_of_Env env -> tex_env ppf env
  | Fail -> pr ppf "\\%sFailTerm" g

let tex_judgment ppf = function
    EvalTo (env, e, v) -> 
      pr ppf "\\%sEvalTo{%a}{%a}{%a}" g tex_env env tex_exp e tex_val v
  | Matches (v, p, r) ->
      pr ppf "\\%sMatches{%a}{%a}{%a}" g tex_val v tex_pat p tex_res r
  | AppBOp (p, v1, v2, v3) -> 
      let op = "\\" ^ g ^ match p with 
	  Plus -> "PlusTerm" | Minus -> "MinusTerm"
	| Mult -> "MultTerm" | Lt -> "LTTerm" 
      in pr ppf "\\%sAppBOp{%a}{%s}{%a}{%a}" g
	   tex_val v1 op tex_val v2 tex_val v3
