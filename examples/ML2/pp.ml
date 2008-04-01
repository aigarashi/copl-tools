open Format
open Core

let pr = fprintf

(* if e is the left operand of e_up, do you need parentheses for e? *)
let (<) e e_up = match e, e_up with
    (* mult associates stronger than plus or minus *)
    BinOp((Plus | Minus | Lt), _, _), BinOp(Mult, _, _) 
  | If(_, _, _),                      BinOp(Mult, _, _)
  | BinOp(Lt, _, _),                  BinOp((Plus | Minus), _, _) 
  | If(_, _, _),                      BinOp((Plus | Minus), _, _)
  | If(_, _, _),                      BinOp(Lt, _, _)
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
 
let with_paren_L ppf_e e_up ppf e =
  if e < e_up then pr ppf "(%a)" ppf_e e else pr ppf "%a" ppf_e e

let with_paren_R ppf_e e_up ppf e =
  if e_up > e then pr ppf "(%a)" ppf_e e else pr ppf "%a" ppf_e e

let rec print_val ppf = function
    Value_of_int i -> pr ppf "%d" i
  | Value_of_Boolean True -> pr ppf "true"
  | Value_of_Boolean False -> pr ppf "false"

let rec print_exp ppf e = match e with
    Exp_of_int i -> pr ppf "%d" i
  | Exp_of_Boolean True -> pr ppf "true"
  | Exp_of_Boolean False -> pr ppf "false"
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

let print_judgment ppf = function
    EvalTo (e, v) -> pr ppf "%a evalto %a" print_exp e print_val v
  | AppBOp (Lt, v1, v2, Value_of_Boolean True) ->
      pr ppf "%a is less than %a" print_val v1 print_val v2
  | AppBOp (Lt, v1, v2, Value_of_Boolean False) ->
      pr ppf "%a is not less than %a" print_val v1 print_val v2
  | AppBOp (p, v1, v2, v3) -> 
      let op = match p with Plus -> "plus" | Minus -> "minus" | Mult -> "mult"
      in pr ppf "%a %s %a is %a" print_val v1 op print_val v2 print_val v3

let print_pjudgment ppf = function
    In_EvalTo e -> pr ppf "%a evalto ?" print_exp e
  | In_AppBOp (Lt, v1, v2) ->
      pr ppf "%a is less than %a ?" print_val v1 print_val v2
  | In_AppBOp (p, v1, v2) -> 
      let op = match p with Plus -> "plus" | Minus -> "minus" | Mult -> "mult"
      in pr ppf "%a %s %a is ?" print_val v1 op print_val v2

let tex_judgment ppf = function
    EvalTo (e, v) -> pr ppf "\\EvalTo{%a}{%a}" print_exp e print_val v
  | AppBOp (p, v1, v2, v3) -> 
      let op = match p with Plus -> "plus" | Minus -> "minus" | Mult -> "mult" | Lt -> "lt" 
      in pr ppf "\\AppBOp{%a}{%s}{%a}{%a}" print_val v1 op print_val v2 print_val v3
    