open Format
open Core

let g = "TypingMLiv"

let pr = fprintf

let is_negative i = (i < 0)

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
  | Let(_, _, _),                     BinOp(_ , _, _)
  | Abs(_, _),                        BinOp(_ , _, _)
  | LetRec(_, _, _, _),               BinOp(_ , _, _)
  | BinOp(_, _, _),                   App(_, _)
  | If(_, _, _),                      App(_, _)
  | Let(_, _, _),                     App(_, _)
  | Abs(_, _),                        App(_, _)
  | LetRec(_, _, _, _),               App(_, _)
      -> true
  | Exp_of_int i,                     App(_, _) when is_negative i -> true
  | _ -> false

(* if e is the right operand of e_up, do you need parentheses for e? *)
let (>) e_up e = match e_up, e with
    (* mult associates stronger than plus or minus,
       bin ops are left-associative, and
       function application, which is left-associative, is the strongest *)
  | App(_, _),                   App(_, _)
  | App(_, _),                   BinOp(_, _, _)
  | App(_, _),                   If(_, _, _)
  | App(_, _),                   Let(_, _, _)
  | App(_, _),                   Abs(_, _)
  | App(_, _),                   LetRec(_, _, _, _)
  | BinOp(Mult, _, _),           BinOp(_, _, _)
  | BinOp((Plus | Minus), _, _), BinOp((Plus | Minus | Lt), _, _)
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
      | Abs(x, e) ->
	  pr ppf "fun %s -> %a" x print_exp e
      | App(e1, e2) ->
	  pr ppf "%a %a" 
	    (with_paren_L print_exp e) e1
	    (with_paren_R print_exp e) e2
      | LetRec(x, y, e1, e2) ->
	  pr ppf "let rec %s = fun %s -> %a in %a" x y
	    print_exp e1
	    print_exp e2

let rec tex_exp ppf e = 
    let with_paren_L = with_paren (<)
    and with_paren_R = with_paren (fun e_up e -> e > e_up) in
      match e with
	  Exp_of_int i -> pr ppf "%d" i
	| Exp_of_bool b -> pp_print_string ppf (string_of_bool b)
	| Exp_of_string id -> pp_print_string ppf id
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
	| Let(x, e1, e2) ->
	    pr ppf "\\%sLetTerm{%s}{%a}{%a}" g
	      x
	      tex_exp e1
	      tex_exp e2
	| Abs(x, e) ->
	    pr ppf "\\%sFunTerm{%s}{%a}" g x tex_exp e
	| App(e1, e2) ->
	    pr ppf "\\%sAppTerm{%a}{%a}" g
	      (with_paren_L tex_exp e) e1
	      (with_paren_R tex_exp e) e2
	| LetRec(x, y, e1, e2) ->
	    pr ppf "\\%sLetRecTerm{%s}{%s}{%a}{%a}" g 
	      x y tex_exp e1 tex_exp e2

(* if t is the left operand of t_up, do you need parentheses for t? *)
let (<) t t_up = match t, t_up with
    (* -> is right associative *)
  | TyFun(_,_),  TyFun(_,_) -> true
  | _ -> false

(* if t is the right operand of t_up, do you need parentheses for t? *)
let (>) t_up t = false

let rec print_type ppf t = 
  let with_paren_L = with_paren (<) 
  and with_paren_R = with_paren (fun e_up e -> e > e_up) in
    match t with
	TyInt -> pp_print_string ppf "int"
      | TyBool -> pp_print_string ppf "bool"
(*      | TyVar a -> pr ppf "'%s" a *)
      | TyFun(t1, t2) -> 
	  pr ppf "%a -> %a"
	    (with_paren_L print_type t) t1
	    (with_paren_R print_type t) t2

let rec tex_type ppf t = 
  let with_paren_L = with_paren (<) 
  and with_paren_R = with_paren (fun e_up e -> e > e_up) in
    match t with
	TyInt -> pr ppf "\\%sTyIntTerm" g
      | TyBool -> pr ppf "\\%sTyBoolTerm" g
(*      | TyVar a -> pr ppf "'%s" a *)
      | TyFun(t1, t2) -> 
	  pr ppf "\\%sTyFunTerm{%a}{%a}" g
	    (with_paren_L tex_type t) t1
	    (with_paren_R tex_type t) t2

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

let rec tex_env ppf = function
    Empty -> ()
  | Bind(env',x,t) -> pr ppf "%a%s : %a" tex_env' env' x tex_type t
and tex_env' ppf = function
  | Empty -> ()
  | Bind(env',x,t) -> pr ppf "%a%s : %a,@ " tex_env' env' x tex_type t

let tex_judgment ppf = function
    Typing (env, e, t) -> 
      pr ppf "\\%sTyping{%a}{%a}{%a}" g tex_env env tex_exp e tex_type t
