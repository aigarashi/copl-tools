open Format
open Core

let g = "MLiii"

let pr = fprintf

let is_negative i = (i < 0)

(* generic functions to generate parens depending on precedence *)
let with_paren lt ppf_e e_up ppf e =
  let (<) = lt in
  if e < e_up then pr ppf "(%a)" ppf_e e else pr ppf "%a" ppf_e e

(* precedence for expressions *)
let rec is_last_longexp = function
    BinOpD(_,_,e) -> is_last_longexp e
  | IfD(_,_,_) | LetD(_,_) | AbsD _ | LetRecD(_,_) -> true
  | _ -> false

(* if e is the left operand of e_up, do you need parentheses for e? *)
let (<) e e_up = match e, e_up with
    (* mult associates stronger than plus or minus *)
    BinOpD((Plus | Minus | Lt), _, _), BinOpD(Mult, _, _)
  | BinOpD(Lt, _, _),                  BinOpD((Plus | Minus), _, _)
  | BinOpD(_, _, _),                   AppD(_, _)
      -> true
  | e,                                 (BinOpD(_, _, _) | AppD(_,_)) when is_last_longexp e
      -> true
  | DBExp_of_int i,                    AppD(_, _) when is_negative i -> true
  | _ -> false

(* if e is the right operand of e_up, do you need parentheses for e? *)
let (>) e_up e = match e_up, e with
    (* mult associates stronger than plus or minus,
       bin ops are left-associative, and
       function application, which is left-associative, is the strongest *)
  | AppD(_, _),                   AppD(_, _)
  | AppD(_, _),                   BinOpD(_, _, _)
  | AppD(_, _),                   IfD(_, _, _)
  | AppD(_, _),                   LetD(_, _)
  | AppD(_, _),                   AbsD _
  | AppD(_, _),                   LetRecD(_, _)
  | BinOpD(Mult, _, _),           BinOpD(_, _, _)
  | BinOpD((Plus | Minus), _, _), BinOpD((Plus | Minus | Lt), _, _)
      -> true
  | AppD(_, _),                   DBExp_of_int i when is_negative i -> true
  | _ -> false

let rec print_dexp ppf e =
    let with_paren_L = with_paren (<)
    and with_paren_R = with_paren (fun e_up e -> e > e_up) in
      match e with
	  DBExp_of_int i -> pr ppf "%d" i
	| DBExp_of_bool true -> pr ppf "true"
	| DBExp_of_bool false -> pr ppf "false"
	| Index id -> pr ppf "#%d" id
	| BinOpD(p, e1, e2) ->
	    let op =
	      match p with Plus -> "+" | Minus -> "-" | Mult -> "*" | Lt -> "<" in
	      pr ppf "%a %s %a"
		(with_paren_L print_dexp e) e1
		op
		(with_paren_R print_dexp e) e2
	| IfD(e1, e2, e3) ->
	    pr ppf "if %a then %a else %a"
	      print_dexp e1
	      print_dexp e2
	      print_dexp e3
	| LetD(e1, e2) ->
	    pr ppf "let . = %a in %a"
	      print_dexp e1
	      print_dexp e2
	| AbsD e ->
	    pr ppf "fun . -> %a" print_dexp e
	| AppD(e1, e2) ->
	    pr ppf "%a %a"
	      (with_paren_L print_dexp e) e1
	      (with_paren_R print_dexp e) e2
	| LetRecD(e1, e2) ->
	    pr ppf "let rec . = fun . -> %a in %a"
	      print_dexp e1
	      print_dexp e2

let rec print_env ppf = function
    Empty -> ()
  | Bind(env', v) -> pr ppf "%a%a" print_env' env' print_val v
and print_env' ppf = function
  | Empty -> ()
  | Bind(env', v) -> pr ppf "%a%a,@ " print_env' env' print_val v

and print_val ppf = function
    DBValue_of_int i -> pr ppf "%d" i
  | DBValue_of_bool true -> pr ppf "true"
  | DBValue_of_bool false -> pr ppf "false"
  | Fun(env, e) -> pr ppf "(%a)[fun . -> %a]" print_env env print_dexp e
  | Rec(env, e) -> pr ppf "(%a)[rec . = fun . -> %a]" print_env env print_dexp e

let print_judgment ppf = function
    EvalTo (env, e, v) ->
      pr ppf "@[@[%a@]@ |- @[%a@] evalto %a@]" print_env env print_dexp e print_val v
  | AppBOp (Lt, v1, v2, DBValue_of_bool true) ->
      pr ppf "@[%a is less than %a@]" print_val v1 print_val v2
  | AppBOp (Lt, v1, v2, DBValue_of_bool false) ->
      pr ppf "@[%a is not less than %a@]" print_val v1 print_val v2
  | AppBOp (p, v1, v2, v3) ->
      let op = match p with Plus -> "plus" | Minus -> "minus" | Mult -> "times"
      in pr ppf "@[%a %s %a is %a@]" print_val v1 op print_val v2 print_val v3

let print_pjudgment ppf = function
    In_EvalTo (env, e) ->
      pr ppf "@[%a@]@ |- %a evalto ?" print_env env print_dexp e
  | In_AppBOp (Lt, v1, v2) ->
      pr ppf "%a is less than %a ?" print_val v1 print_val v2
  | In_AppBOp (p, v1, v2) ->
      let op = match p with Plus -> "plus" | Minus -> "minus" | Mult -> "times"
      in pr ppf "%a %s %a is ?" print_val v1 op print_val v2

let rec tex_dexp ppf e =
    let with_paren_L = with_paren (<)
    and with_paren_R = with_paren (fun e_up e -> e > e_up) in
      match e with
	  DBExp_of_int i -> pr ppf "%d" i
	| DBExp_of_bool b -> pp_print_string ppf (string_of_bool b)
	| Index id -> pr ppf "#%d" id
	| BinOpD(p, e1, e2) ->
	    let op =
	      match p with Plus -> "+" | Minus -> "-" | Mult -> "*" | Lt -> "<" in
	      pr ppf "\\%sBinOpDTerm{%a}{%s}{%a}" g
		(with_paren_L tex_dexp e) e1
		op
		(with_paren_R tex_dexp e) e2
	| IfD(e1, e2, e3) ->
	    pr ppf "\\%sIfDTerm{%a}{%a}{%a}" g
	      tex_dexp e1
	      tex_dexp e2
	      tex_dexp e3
	| LetD(e1, e2) ->
	    pr ppf "\\%sLetDTerm{%a}{%a}" g
	      tex_dexp e1
	      tex_dexp e2
	| AbsD e ->
	    pr ppf "\\%sFunDTerm{%a}" g tex_dexp e
	| AppD(e1, e2) ->
	    pr ppf "\\%sAppDTerm{%a}{%a}" g
	      (with_paren_L tex_dexp e) e1
	      (with_paren_R tex_dexp e) e2
	| LetRecD(e1, e2) ->
	    pr ppf "\\%sLetRecTerm{%a}{%a}" g
	      tex_dexp e1 tex_dexp e2

let rec tex_env ppf = function
    Empty -> ()
  | Bind(env', v) -> pr ppf "%a%a" tex_env' env' tex_val v
and tex_env' ppf = function
  | Empty -> ()
  | Bind(env', v) -> pr ppf "%a%a,@ " tex_env' env' tex_val v

and tex_val ppf = function
    DBValue_of_int i -> pr ppf "%d" i
  | DBValue_of_bool b -> pp_print_string ppf (string_of_bool b)
  | Fun(env, e) -> pr ppf "\\%sFunTerm{%a}{%a}" g tex_env env tex_dexp e
  | Rec(env, e) -> pr ppf "\\%sRecTerm{%a}{%a}" g
      tex_env env tex_dexp e

let tex_judgment ppf = function
    EvalTo (env, e, v) ->
      pr ppf "\\%sEvalTo{%a}{%a}{%a}" g tex_env env tex_dexp e tex_val v
  | AppBOp (p, v1, v2, v3) ->
      let op = "\\" ^ g ^ match p with
	  Plus -> "PlusTerm" | Minus -> "MinusTerm"
	| Mult -> "MultTerm" | Lt -> "LTTerm"
      in pr ppf "\\%sAppBOp{%a}{%s}{%a}{%a}" g
	   tex_val v1 op tex_val v2 tex_val v3
