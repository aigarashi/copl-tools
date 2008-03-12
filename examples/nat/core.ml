type nat = Z | S of nat
and exp = Exp_of_Nat of nat | P of exp * exp | M of exp * exp

type judgment = EvalTo of exp * nat | PlusIs of nat * nat * nat |
              MultIs of nat * nat * nat

type rulename = string

type rule = {
  name : rulename;
  conc : judgment;
  premises : judgment;
}  

type derivation = {
  conc:  judgment;
  by:    rulename;
  since: derivation list
}
let rec deriv_check = function
| {conc = _conc_; by = "E-Const"; since = _derivs_} ->
      (match _conc_ with
         EvalTo(Exp_of_Nat n, _n) when _n = n && true ->
         (match _derivs_ with
            [] ->
            if true then _conc_
            else failwith "Wrong rule application: E-Const"
          | _ -> failwith "The number of premises is wrong: E-Const")
       | _ -> failwith "The form of conclusion is wrong: E-Const")
| {conc = _conc_; by = "E-Plus"; since = _derivs_} ->
      (match _conc_ with
         EvalTo(P(e1, e2), n) when  true ->
         (match _derivs_ with
            [_d1_; _d2_; _d3_; ] ->
            (match deriv_check _d1_ with
                 EvalTo(_e1, _n1) when  true ->
                 (match deriv_check _d2_ with
                      EvalTo(__e2, __n2) when  true ->
                      (match deriv_check _d3_ with
                           PlusIs(___n1, ___n2, ___n) when  true ->
                           if ___n = n && _e1 = e1 && __e2 = e2 &&
                              ___n1 = _n1 && ___n2 = __n2 && true
                           then _conc_
                           else failwith "Wrong rule application: E-Plus"
                       | _ -> failwith "The form of premise is wrong: E-Plus")
                  | _ -> failwith "The form of premise is wrong: E-Plus")
             | _ -> failwith "The form of premise is wrong: E-Plus")
          | _ -> failwith "The number of premises is wrong: E-Plus")
       | _ -> failwith "The form of conclusion is wrong: E-Plus")
| {conc = _conc_; by = "E-Mult"; since = _derivs_} ->
      (match _conc_ with
         EvalTo(M(e1, e2), n) when  true ->
         (match _derivs_ with
            [_d1_; _d2_; _d3_; ] ->
            (match deriv_check _d1_ with
                 EvalTo(_e1, _n1) when  true ->
                 (match deriv_check _d2_ with
                      EvalTo(__e2, __n2) when  true ->
                      (match deriv_check _d3_ with
                           MultIs(___n1, ___n2, ___n) when  true ->
                           if ___n = n && _e1 = e1 && __e2 = e2 &&
                              ___n1 = _n1 && ___n2 = __n2 && true
                           then _conc_
                           else failwith "Wrong rule application: E-Mult"
                       | _ -> failwith "The form of premise is wrong: E-Mult")
                  | _ -> failwith "The form of premise is wrong: E-Mult")
             | _ -> failwith "The form of premise is wrong: E-Mult")
          | _ -> failwith "The number of premises is wrong: E-Mult")
       | _ -> failwith "The form of conclusion is wrong: E-Mult")
| {conc = _conc_; by = "P-Zero"; since = _derivs_} ->
      (match _conc_ with
         PlusIs(Z, n, _n) when _n = n && true ->
         (match _derivs_ with
            [] ->
            if true then _conc_
            else failwith "Wrong rule application: P-Zero"
          | _ -> failwith "The number of premises is wrong: P-Zero")
       | _ -> failwith "The form of conclusion is wrong: P-Zero")
| {conc = _conc_; by = "P-Succ"; since = _derivs_} ->
      (match _conc_ with
         PlusIs(S(n1), n2, S(n)) when  true ->
         (match _derivs_ with
            [_d1_; ] ->
            (match deriv_check _d1_ with
                 PlusIs(_n1, _n2, _n) when  true ->
                 if _n = n && _n1 = n1 && _n2 = n2 && true then _conc_
                 else failwith "Wrong rule application: P-Succ"
             | _ -> failwith "The form of premise is wrong: P-Succ")
          | _ -> failwith "The number of premises is wrong: P-Succ")
       | _ -> failwith "The form of conclusion is wrong: P-Succ")
| {conc = _conc_; by = "M-Zero"; since = _derivs_} ->
      (match _conc_ with
         MultIs(Z, n, Z) when  true ->
         (match _derivs_ with
            [] ->
            if true then _conc_
            else failwith "Wrong rule application: M-Zero"
          | _ -> failwith "The number of premises is wrong: M-Zero")
       | _ -> failwith "The form of conclusion is wrong: M-Zero")
| {conc = _conc_; by = "M-Succ"; since = _derivs_} ->
      (match _conc_ with
         MultIs(S(n1), n2, n4) when  true ->
         (match _derivs_ with
            [_d1_; _d2_; ] ->
            (match deriv_check _d1_ with
                 MultIs(_n1, _n2, _n3) when  true ->
                 (match deriv_check _d2_ with
                      PlusIs(__n2, __n3, __n4) when  true ->
                      if _n1 = n1 && __n2 = _n2 && __n2 = n2 && __n3 = _n3 &&
                         __n4 = n4 && true
                      then _conc_
                      else failwith "Wrong rule application: M-Succ"
                  | _ -> failwith "The form of premise is wrong: M-Succ")
             | _ -> failwith "The form of premise is wrong: M-Succ")
          | _ -> failwith "The number of premises is wrong: M-Succ")
       | _ -> failwith "The form of conclusion is wrong: M-Succ")
| {by=name} -> failwith ("No such rule: " ^ name)
