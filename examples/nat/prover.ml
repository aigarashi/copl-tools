(* Nat, Exp, n: Nat, e: Exp, Z: Nat[], S: Nat[Nat], Nat <: Exp,
   P: Exp[Exp, Exp], M: Exp[Exp, Exp], EvalTo: j[Exp, Nat],
   PlusIs: j[Nat, Nat, Nat], MultIs: j[Nat, Nat, Nat] *)

open MySupport.Error
open Derivation
type nat = Z | S of nat
and exp = Exp_of_Nat of nat | P of exp * exp | M of exp * exp

type judgment = EvalTo of exp * nat | PlusIs of nat * nat * nat
              | MultIs of nat * nat * nat
type in_judgment =
    In_EvalTo of exp | In_PlusIs of nat * nat | In_MultIs of nat * nat

let dummy = Lexing.dummy_pos

let rec make_deriv = function
| In_EvalTo(Exp_of_Nat n) -> 
    let _conc_ = EvalTo(Exp_of_Nat n, n) in
      {conc = _conc_; by = "E-Const"; since = []; pos = dummy }
| In_EvalTo(P(e1, e2)) ->
    let _d1_ = make_deriv (In_EvalTo e1) in
    (match _d1_.conc with
	EvalTo(_, n1) ->
	  let _d2_ = make_deriv (In_EvalTo e2) in
	    (match _d2_.conc with
		EvalTo(_, n2) -> 
		  let _d3_ = make_deriv (In_PlusIs (n1, n2)) in
		    (match _d3_.conc with
			PlusIs(_,_,n) ->
			  let _conc_ = EvalTo(P(e1, e2), n) in
			    {conc = _conc_; by = "E-Plus"; 
			     since = [_d1_; _d2_; _d3_]; pos = dummy }
			      (* error handling omitted *)
		    )))
| In_EvalTo(M(e1, e2)) ->
    let _d1_ = make_deriv (In_EvalTo e1) in
    let EvalTo(_, n1) = _d1_.conc in
    let _d2_ = make_deriv (In_EvalTo e2) in
    let EvalTo(_, n2) = _d2_.conc in
    let _d3_ = make_deriv (In_MultIs (n1, n2)) in
    let MultIs(_,_,n) = _d3_.conc in
    let _conc_ = EvalTo(M(e1, e2), n) in
      {conc = _conc_; by = "E-Mult"; since = [_d1_; _d2_; _d3_]; pos = dummy }
| In_PlusIs(Z, n) ->
    let _conc_ = PlusIs(Z, n, n) in
      {conc = _conc_; by = "P-Zero"; since = []; pos = dummy }
| In_PlusIs(S(n1), n2) ->
    let _d1_ = make_deriv (In_PlusIs(n1, n2)) in
    let PlusIs(_,_,n) = _d1_.conc in
    let _conc_ = PlusIs(S(n1), n2, n) in
      {conc = _conc_; by = "P-Succ"; since = [_d1_]; pos = dummy }
| In_MultIs(Z, n) ->
    let _conc_ = MultIs(Z, n, Z) in
      {conc = _conc_; by = "M-Zero"; since = []; pos = dummy }
| In_MultIs(S(n1), n2) ->
    let _d1_ = make_deriv (In_MultIs(n1, n2)) in
    let MultIs(_,_,n3) = _d1_.conc in
    let _d2_ = make_deriv (In_PlusIs(n2, n3)) in
    let PlusIs(_,_,n4) = _d2_.conc in
    let _conc_ = MultIs(S(n1), n2, n4) in
      {conc = _conc_; by = "M-Succ"; since = [_d1_; _d2_]; pos = dummy }

