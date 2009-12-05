open Format
open Core

let pr = fprintf

let is_negative i = (i < 0)

(* generic functions to generate parens depending on precedence *)
let with_paren lt ppf_e e_up ppf e =
  let (<) = lt in
  if e < e_up then pr ppf "(%a)" ppf_e e else pr ppf "%a" ppf_e e

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
  | Match(_, _, _, _, _),             BinOp(_, _, _)
  | BinOp(_, _, _),                   App(_, _)
  | If(_, _, _),                      App(_, _)
  | Let(_, _, _),                     App(_, _)
  | Abs(_, _),                        App(_, _)
  | LetRec(_, _, _, _),               App(_, _)
  | Match(_, _, _, _, _),             App(_, _)
  | Cons(_, _),                       App(_, _)
  | BinOp(Lt, _, _),                  Cons(_,_)
  | If(_, _, _),                      Cons(_, _)
  | Let(_, _, _),                     Cons(_, _)
  | Abs(_, _),                        Cons(_, _)
  | LetRec(_, _, _, _),               Cons(_, _)
  | Cons(_,_),                        Cons(_, _)
  | Match(_, _, _, _, _),             Cons(_, _)
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
      | Cons(e1, e2) -> pr ppf "%a :: %a" 
	    (with_paren_L print_exp e) e1 
	    (with_paren_R print_exp e) e2
      | Match(e1, e2, Var x, Var y, e3) ->
	  pr ppf "match %a with [] -> %a | %s :: %s -> %a"
	    print_exp e1
	    print_exp e2
	    x
	    y
	    print_exp e3

(* if t is the left operand of t_up, do you need parentheses for t? *)
let (<) t t_up = match t, t_up with
    (* -> is right associative,
       list binds tighter than ->
    *)
  | TyFun(_,_),  TyFun(_,_) 
  | TyFun(_,_),  TyList _
      -> true
  | _ -> false

(* if t is the right operand of t_up, do you need parentheses for t? *)
let (>) t_up t = false 

let rec print_type ppf t = 
  let with_paren_L = with_paren (<) 
  and with_paren_R = with_paren (fun e_up e -> e > e_up) in
    match t with
	TyInt -> pp_print_string ppf "int"
      | TyBool -> pp_print_string ppf "bool"
      | TyFun(t1, t2) -> 
	  pr ppf "%a -> %a"
	    (with_paren_L print_type t) t1
	    (with_paren_R print_type t) t2
      | TyList t1 ->
	  pr ppf "%a list" (with_paren_L print_type t) t1

let rec print_env ppf = function
    Empty -> ()
  | Bind(env', Var x, t) -> pr ppf "%a%s : %a" print_env' env' x print_type t
and print_env' ppf = function
  | Empty -> ()
  | Bind(env', Var x, t) -> pr ppf "%a%s : %a,@ " print_env' env' x print_type t

let print_judgment ppf = function
    Typing (env, e, t) -> 
      pr ppf "@[@[%a@]@ |- @[%a@]@ : %a@]" print_env env print_exp e print_type t

let print_pjudgment ppf = function
    In_Typing (env, e) ->
      pr ppf "@[%a@]@ |- %a : ?" print_env env print_exp e 

let tex_judgment ppf = function
    Typing (env, e, t) -> 
      pr ppf "\\Typing{%a}{%a}{%a}" print_env env print_exp e print_type t
