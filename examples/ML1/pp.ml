open Core
open Format

let pr = fprintf

(* if e1 is the left operand of e2, do you need parentheses for e1? *)
let (<) e1 e2 = match e1, e2 with
    (* mult associates stronger than plus or minus *)
    BinOp((Plus | Minus), _, _), BinOp(Mult, _, _) -> true
  | _ -> false

(* if e1 is the right operand of e2, do you need parentheses for e1? *)
let (>) e1 e2 = match e1, e2 with
    (* mult associates stronger than plus or minus,
       and bin ops are left-associative *)
    BinOp(_, _, _), BinOp((Plus | Minus), _, _) 
  | BinOp(Mult, _, _), BinOp(Mult, _, _) -> true
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
      let op = match p with Plus -> "+" | Minus -> "-" | Mult -> "*" in
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
  | AppBOp (p, v1, v2, v3) -> 
      let op = match p with Plus -> "plus" | Minus -> "minus" | Mult -> "mult"
      in pr ppf "%a %s %a is %a" print_val v1 op print_val v2 print_val v3
