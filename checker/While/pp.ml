open Format
open Core

let g = "While"

let pr = fprintf

(* generic functions to generate parens depending on precedence *)
let with_paren lt ppf_e e_up ppf e =
  let (<) = lt in
  if e < e_up then pr ppf "(%a)" ppf_e e else pr ppf "%a" ppf_e e

module AExp =
struct
(* if e is the left operand of e_up, do you need parentheses for e? *)
  let (<) e e_up = match e, e_up with
    (* mult associates stronger than plus or minus *)
      AOp((Plus | Minus), _, _), AOp(Mult, _, _) 
	-> true
    | _ -> false

  (* if e is the right operand of e_up, do you need parentheses for e? *)
  let (>) e_up e = match e_up, e with
    (* mult associates stronger than plus or minus,
       and bin ops are left-associative *)
    AOp(Mult, _, _),           AOp(_, _, _)
  | AOp((Plus | Minus), _, _), AOp((Plus | Minus), _, _)
      -> true
  | _ -> false

  let rec print ppf a =
    let with_paren_L = with_paren (<)
    and with_paren_R = with_paren (fun a_up a -> a > a_up) in
    match a with
      AExp_of_int i -> pr ppf "%d" i
    | AExp_of_Var (Var id) -> pp_print_string ppf id
    | AOp(aop, a1, a2) ->
      let op = match aop with Plus -> "+" | Minus -> "-" | Mult -> "*" in
      pr ppf "%a %s %a"
	(with_paren_L print a) a1
	op
	(with_paren_R print a) a2

  let rec tex ppf a =
    let with_paren_L = with_paren (<)
    and with_paren_R = with_paren (fun a_up a -> a > a_up) in
    match a with
      AExp_of_int i -> pr ppf "%d" i
    | AExp_of_Var (Var id) -> pp_print_string ppf id
    | AOp(aop, a1, a2) ->
      let op = match aop with Plus -> "+" | Minus -> "-" | Mult -> "*" in
      pr ppf "\\%sAOpTerm{%a}{%s}{%a}" g
	(with_paren_L tex a) a1
	op
	(with_paren_R tex a) a2
end

module BExp =
struct
(* if e is the (left) operand of e_up, do you need parentheses for e? *)
  let (<) e e_up = match e, e_up with
      LOp(Or,_,_), LOp(And,_,_) 
    | LOp(_,_,_),   Not _
      -> true
    | _ -> false

(* if e is the (right) operand of e_up, do you need parentheses for e? *)
  let (>) e_up e = match e_up, e with
      LOp(And,_,_), LOp((And|Or),_, _)
    | LOp(Or,_,_),  LOp(Or,_,_)
      -> true
    | _ -> false

  let rec print ppf b =
    let with_paren_L = with_paren (<)
    and with_paren_R = with_paren (fun e_up e -> e > e_up) in
    match b with
      BExp_of_bool b -> pp_print_string ppf (string_of_bool b)
    | Not b0 -> pr ppf "!%a" (with_paren_L print b) b0
    | LOp(p, b1, b2) ->
      let op = match p with And -> "&&" | Or -> "||" in
      pr ppf "%a %s %a"
	(with_paren_L print b) b1 
	op
	(with_paren_R print b) b2
    | COp(p, a1, a2) ->
      let op = match p with Lt -> "<" | Eq -> "=" | Le -> "<=" in
      pr ppf "%a %s %a"
	AExp.print a1  op  AExp.print a2

  let rec tex ppf b =
    let with_paren_L = with_paren (<)
    and with_paren_R = with_paren (fun e_up e -> e > e_up) in
    match b with
      BExp_of_bool b -> pp_print_string ppf (string_of_bool b)
    | Not b0 -> pr ppf "\\%sNotTerm{%a}" g (with_paren_L tex b) b0
    | LOp(p, b1, b2) ->
      let op = match p with And -> "\\&\\&" | Or -> "||" in
      pr ppf "\\%sLOpTerm{%a}{%s}{%a}" g
	(with_paren_L tex b) b1 
	op
	(with_paren_R tex b) b2
    | COp(p, a1, a2) ->
      let op = match p with Lt -> "\\lt" | Eq -> "=" | Le -> "\\leq" in
      pr ppf "\\%sCOpTerm{%a}{%s}{%a}" g
	AExp.tex a1  op  AExp.tex a2

end

module Comm =
struct
(* if e is the (left) operand of e_up, do you need parentheses for e? *)
  let (<) e e_up = match e, e_up with
      (If(_,_,_) | While(_,_)), Seq(_,_) -> true
    | _ -> false

(* if e is the (right) operand of e_up, do you need parentheses for e? *)
  let (>) e_up e = match e_up, e with
      Seq(_,_), Seq(_,_) -> true
    | _ -> false

  let rec print ppf c =
    let with_paren_L = with_paren (<)
    and with_paren_R = with_paren (fun c_up c -> c > c_up) in
      match c with
	Skip -> pp_print_string ppf "skip"
      | Assign(Var id, a) -> pr ppf "%s := %a" id AExp.print a
      | Seq(c1, c2) -> pr ppf "%a; %a"
	(with_paren_L print c) c1
	(with_paren_R print c) c2
      | If(b, c1, c2) ->
	pr ppf "if %a then %a else %a"
	  BExp.print b
	  print c1
	  print c2
      | While(b, c0) ->
	pr ppf "while (%a) do %a"
	  BExp.print b
	  print c0

  let rec tex ppf c =
    let with_paren_L = with_paren (<)
    and with_paren_R = with_paren (fun c_up c -> c > c_up) in
      match c with
	Skip -> pp_print_string ppf "\\%sSkipTerm"
      | Assign(Var id, a) -> pr ppf "\\%sAssignTerm{%s}{%a}" g id AExp.tex a
      | Seq(c1, c2) -> pr ppf "\\%sSeqTerm{%a}{%a}" g
	(with_paren_L tex c) c1
	(with_paren_R tex c) c2
      | If(b, c1, c2) ->
	pr ppf "\\%sIfTerm{%a}{%a}{%a}" g
	  BExp.tex b
	  tex c1
	  tex c2
      | While(b, c0) ->
	pr ppf "\\%sWhileTerm{%a}{%a}" g
	  BExp.tex b
	  tex c0
end

let rec print_st ppf = function
    Empty -> ()
  | Bind(st',Var x,i) -> pr ppf "%a%s = %d" print_st' st' x i
and print_st' ppf = function
  | Empty -> ()
  | Bind(st',Var x,i) -> pr ppf "%a%s = %d,@ " print_st' st' x i

let print_judgment ppf = function
    AEvalTo(st, a, i) -> 
      pr ppf "@[@[%a@]@ |- @[%a@]@ evalto %d@]" print_st st AExp.print a i
  | BEvalTo(st, b, bv) ->
    pr ppf "@[@[%a@]@ |- @[%a@]@ evalto %b@]" print_st st BExp.print b bv
  | Exec(st1, c, st2) -> 
    pr ppf "@[@[%a@]@ changes @[%a@]@ to @[%a@]@]" 
      Comm.print c print_st st1 print_st st2

let print_pjudgment ppf j = pr ppf "Pretty printer not implemented"

let tex_st = print_st

let tex_judgment ppf = function
    AEvalTo(st, a, i) ->
      pr ppf "\\%sAEvalTo{%a}{%a}{%d}" g tex_st st AExp.tex a i
  | BEvalTo(st, b, bv) ->
      pr ppf "\\%sBEvalTo{%a}{%a}{%b}" g tex_st st BExp.tex b bv
  | Exec(st1, c, st2) ->
      pr ppf "\\%sExec{%a}{%a}{%a}" g tex_st st1 Comm.tex c tex_st st2

