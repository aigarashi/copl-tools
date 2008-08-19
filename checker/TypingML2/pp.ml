open Format
open Core

let pr = fprintf

(* generic functions to generate parens depending on precedence *)
let with_paren lt ppf_e e_up ppf e =
  let (<) = lt in
  if e < e_up then pr ppf "(%a)" ppf_e e else pr ppf "%a" ppf_e e

(* precedence for expressions *)
(* if e is the left operand of e_up, do you need parentheses for e? *)
let (<) e e_up = match e, e_up with
    (* mult associates stronger than plus or minus *)
    BinOp((Plus | Minus | Lt), _, _), BinOp(Mult, _, _) 
  | BinOp(Lt, _, _),                  BinOp((Plus | Minus), _, _) 
  | If(_, _, _),                      BinOp(_, _, _)
  | Let(_, _, _),                     BinOp(_, _, _)
      -> true
  | _ -> false

(* if e is the right operand of e_up, do you need parentheses for e? *)
let (>) e_up e = match e_up, e with
    (* mult associates stronger than plus or minus,
       and bin ops are left-associative *)
    BinOp(Mult, _, _),           BinOp(_, _, _)
  | BinOp((Plus | Minus), _, _), BinOp((Plus | Minus | Lt), _, _)
      -> true
  | _ -> false
 
let rec print_exp ppf e =
    let with_paren_L = with_paren (<)
    and with_paren_R = with_paren (fun e_up e -> e > e_up) in
      match e with
	  Exp_of_int i -> pr ppf "%d" i
	| Exp_of_bool true -> pr ppf "true"
	| Exp_of_bool false -> pr ppf "false"
	| Exp_of_string id -> pp_print_string ppf id
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
	| Let(x, e1, e2) ->
	    pr ppf "let %s = %a in %a"
	      x
	      print_exp e1
	      print_exp e2

let print_type ppf = function
    TyInt -> pp_print_string ppf "int"
  | TyBool -> pp_print_string ppf "bool"

let rec print_env ppf = function
    Empty -> ()
  | Bind(env',x,t) -> pr ppf "%a%s : %a" print_env' env' x print_type t
and print_env' ppf = function
  | Empty -> ()
  | Bind(env',x,t) -> pr ppf "%a%s : %a,@ " print_env' env' x print_type t

let print_judgment ppf = function
    Typing (env, e, t) -> 
      pr ppf "@[@[%a@]@ |-@ @[@[%a@]@ : %a@]@]" print_env env print_exp e print_type t

let print_pjudgment ppf = function
    In_Typing (env, e) ->
      pr ppf "@[%a@]@ |- %a : ?" print_env env print_exp e 

let tex_judgment ppf = function
    Typing (env, e, t) -> 
      pr ppf "\\Typing{%a}{%a}{%a}" print_env env print_exp e print_type t