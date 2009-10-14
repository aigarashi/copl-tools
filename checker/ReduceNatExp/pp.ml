open Format
open Core

let g = "EvalNatExp"

let pr = fprintf

let (<) e1 e2 = match e1, e2 with
    P(_,_), M(_,_) -> true
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
    OneStepTo (e, e') -> pr ppf "%a ---> %a" print_exp e print_exp e'
  | MultiStepTo (e, e') -> pr ppf "%a -*-> %a" print_exp e print_exp e'
  | OneStepToD (e, e') -> pr ppf "%a -d-> %a" print_exp e print_exp e'
  | PlusIs (n1, n2, n3) -> 
      pr ppf "%a plus %a is %a" print_nat n1 print_nat n2 print_nat n3
  | MultIs (n1, n2, n3) ->
      pr ppf "%a times %a is %a" print_nat n1 print_nat n2 print_nat n3

let print_pjudgment ppf = function
    In_OneStepTo(e, e') -> pr ppf "%a ---> %a" print_exp e print_exp e'
  | In_MultiStepTo(e, e') -> pr ppf "%a -*-> %a" print_exp e print_exp e'
  | In_OneStepToD e -> pr ppf "%a -d-> ?" print_exp e
  | In_PlusIs (n1, n2) -> 
      pr ppf "%a plus %a is ?" print_nat n1 print_nat n2
  | In_MultIs (n1, n2) ->
      pr ppf "%a times %a is ?" print_nat n1 print_nat n2

let rec tex_nat ppf = function
    Z -> pr ppf "\\%sZTerm" g
  | S n -> pr ppf "\\%sSTerm{%a}" g tex_nat n

let rec tex_exp ppf e = match e with
    Exp_of_Nat n -> tex_nat ppf n
  | P (e1, e2) -> 
      pr ppf "\\%sPTerm{%a}{%a}" g
	(with_paren tex_exp e) e1 
	(with_paren tex_exp e) e2
  | M (e1, e2) -> 
      pr ppf "\\%sMTerm{%a}{%a}" g
	(with_paren tex_exp e) e1 
	(with_paren tex_exp e) e2

let tex_judgment ppf = function
    OneStepTo (e, e') -> pr ppf "\\%sOneStepTo{%a}{%a}" g tex_exp e tex_exp e'
  | MultiStepTo (e, e') -> pr ppf "\\%sMultiStepTo{%a}{%a}" g tex_exp e tex_exp e'
  | OneStepToD (e, e') -> pr ppf "\\%sOneStepToD{%a}{%a}" g tex_exp e tex_exp e'
  | PlusIs (n1, n2, n3) -> 
      pr ppf "\\%sPlusIs{%a}{%a}{%a}" g tex_nat n1 tex_nat n2 tex_nat n3
  | MultIs (n1, n2, n3) ->
      pr ppf "\\%sMultIs{%a}{%a}{%a}" g tex_nat n1 tex_nat n2 tex_nat n3
