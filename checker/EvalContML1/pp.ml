open Format
open Core

let g = "MLi"

let pr = fprintf

(* generic functions to generate parens depending on precedence *)
let with_paren lt ppf_e e_up ppf e =
  let (<) = lt in
  if e < e_up then pr ppf "(%a)" ppf_e e else pr ppf "%a" ppf_e e

(* precedence for expressions *)
let rec is_last_longexp = function
    BinOp(_,_,e) -> is_last_longexp e
  | If(_,_,_) -> true
  | _ -> false

(* if e is the left operand of e_up, do you need parentheses for e? *)
let (<) e e_up = match e, e_up with
    (* mult associates stronger than plus or minus *)
    BinOp((Plus | Minus | Lt), _, _), BinOp(Mult, _, _)
  | BinOp(Lt, _, _),                  BinOp((Plus | Minus), _, _)
      -> true
  | e,                                BinOp(_,_,_) when is_last_longexp e
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
	| Exp_of_bool b -> pp_print_string ppf (string_of_bool b)
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

let print_val ppf = function
    Value_of_int i -> pr ppf "%d" i
  | Value_of_bool b -> pp_print_string ppf (string_of_bool b)

let rec print_cont =
  let with_paren_R = with_paren (fun e_up e -> e > e_up) in
    fun ppf -> function
	RetK -> pr ppf "_"
      | EvalRK(e, p, k) ->
	  let op = match p with Plus -> "+" | Minus -> "-" | Mult -> "*" | Lt -> "<" in
	    pr ppf "@[{_ %s %a}@]%a"
	      op
	      (with_paren_R print_exp (BinOp(p,Exp_of_int 1,e))) e
	      print_optcont k
      | AppOpK(v, p, k) ->
	  let op = match p with Plus -> "+" | Minus -> "-" | Mult -> "*" | Lt -> "<" in
	    pr ppf "@[{%a %s _}@]%a"
	      print_val v
	      op
	      print_optcont k
      | BranchK(e1, e2, k) ->
	  pr ppf "@[{if _ then %a else %a}@]%a"
	    print_exp e1
	    print_exp e2
	    print_optcont k

and print_optcont ppf = function
    RetK -> ()
  | k -> pr ppf "@ @[>> %a@]" print_cont k


let print_judgment ppf = function
    EvalTo (k, e, v) ->
      pr ppf "@[@[@[%a@]@[%a@]@ evalto %a@]@]"
	print_exp e
	print_optcont k
	print_val v
  | AppBOp (Lt, v1, v2, Value_of_bool true) ->
      pr ppf "@[%a is less than %a@]" print_val v1 print_val v2
  | AppBOp (Lt, v1, v2, Value_of_bool false) ->
      pr ppf "@[%a is not less than %a@]" print_val v1 print_val v2
  | AppBOp (p, v1, v2, v3) ->
      let op = match p with Plus -> "plus" | Minus -> "minus" | Mult -> "times"
      in pr ppf "@[%a %s %a is %a@]" print_val v1 op print_val v2 print_val v3
  | AppK (k, v1, v2) ->
      pr ppf "@[%a => %a evalto %a@]" print_val v1 print_cont k print_val v2

let print_pjudgment ppf = function
    In_EvalTo (k, e) ->
      pr ppf "|- %a >> %a evalto ?" print_cont k print_exp e
  | In_AppBOp (Lt, v1, v2) ->
      pr ppf "@[%a is less than %a ?@]" print_val v1 print_val v2
  | In_AppBOp (p, v1, v2) ->
      let op = match p with Plus -> "plus" | Minus -> "minus" | Mult -> "times"
      in pr ppf "@[%a %s %a is ?@]" print_val v1 op print_val v2
  | In_AppK (k, v) ->
      pr ppf "@[%a => %a evalto ?@]" print_val v print_cont k

let rec tex_exp ppf e =
    let with_paren_L = with_paren (<)
    and with_paren_R = with_paren (fun e_up e -> e > e_up) in
      match e with
	  Exp_of_int i -> pr ppf "%d" i
	| Exp_of_bool b -> pp_print_string ppf (string_of_bool b)
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

let tex_val ppf = function
    Value_of_int i -> pr ppf "%d" i
  | Value_of_bool b -> pp_print_string ppf (string_of_bool b)

let rec tex_cont ppf = function
    RetK -> pr ppf "\\%sRetKTerm{}" g
  | EvalRK(e, p, k) ->
      let op = match p with Plus -> "+" | Minus -> "-" | Mult -> "*" | Lt -> "<" in
	pr ppf "\\%sEvalRKTerm{%a}{%s}{%a}" g  tex_exp e  op  tex_cont k
  | AppOpK(v, p, k) ->
      let op = match p with Plus -> "+" | Minus -> "-" | Mult -> "*" | Lt -> "<" in
	pr ppf "\\%sAppOpKTerm{%a}{%s}{%a}" g
	  tex_val v  op  tex_cont k
  | BranchK(e1, e2, k) ->
      pr ppf "\\%sBranchKTerm{%a}{%a}{%a}" g  tex_exp e1  tex_exp e2  tex_cont k

let tex_judgment ppf = function
    EvalTo (k, e, v) ->
      pr ppf "\\%sEvalTo{%a}{%a}{%a}" g  tex_cont k   tex_exp e   tex_val v
  | AppBOp (p, v1, v2, v3) ->
      let op = match p with
	  Plus -> "\\MLiPlusTerm" | Minus -> "\\MLiMinusTerm"
	| Mult -> "\\MLiMultTerm" | Lt -> "\\MLiLTTerm"
      in pr ppf "\\%sAppBOp{%a}{%s}{%a}{%a}"
	   g tex_val v1 op tex_val v2 tex_val v3
  | AppK (k, v1, v2) ->
      pr ppf "\\%sAppK{%a}{%a}{%a}" g tex_cont k tex_val v1 tex_val v2
