open Core
open Format

let pr = fprintf

let (<) e1 e2 = match e1, e2 with
     -> true
  | _ -> false
 
let with_paren ppf_e e_up ppf e =
  if e < e_up then pr ppf "(%a)" ppf_e e else pr ppf "%a" ppf_e e

let rec print_nat ppf = function
    Z -> pr ppf "Z"
  | S n -> pr ppf "S(%a)" print_nat n

let rec print_exp ppf e = match e with
    Exp_of_Nat n -> print_nat ppf n
  | P (e1, e2) -> 
      pr ppf "%a + %a" 
	(with_paren print_exp e) e1 
	(with_paren print_exp e) e2
  | M (e1, e2) -> 
      pr ppf "%a * %a" 
	(with_paren print_exp e) e1 
	(with_paren print_exp e) e2

let print_judgment ppf = function
    EvalTo (e, n) -> pr ppf "%a evalto %a" print_exp e print_nat n
  | PlusIs (n1, n2, n3) -> 
      pr ppf "%a plus %a is %a" print_nat n1 print_nat n2 print_nat n3
  | MultIs (n1, n2, n3) ->
      pr ppf "%a mult %a is %a" print_nat n1 print_nat n2 print_nat n3
