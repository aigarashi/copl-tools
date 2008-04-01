(* int, Boolean, string, Value, Exp, Prim, i: int, b: Boolean, x: string,
   v: Value, e: Exp, p: Prim, True: Boolean[], False: Boolean[],
   int <: Value, Boolean <: Value, int <: Exp, Boolean <: Exp,
   BinOp: Exp[Prim, Exp, Exp], If: Exp[Exp, Exp, Exp], Plus: Prim[],
   Minus: Prim[], Mult: Prim[], Lt: Prim[], EvalTo: j[Exp, Value],
   AppBOp: j[Prim, Value, Value, Value] *)

open MySupport.Error
open Derivation
type boolean = True | False
and value = Value_of_int of int | Value_of_Boolean of boolean
and exp = Exp_of_int of int | Exp_of_Boolean of boolean | BinOp of prim * exp
        * exp | If of exp * exp * exp
and prim = Plus | Minus | Mult | Lt

type judgment = EvalTo of exp * value
              | AppBOp of prim * value * value * value


let rec deriv_check = function
| {conc = _conc_; by = "E-Int"; since = _derivs_; pos = _p_} ->
      (match _conc_ with
         EvalTo(Exp_of_int i, Value_of_int _i) when _i = i && true ->
         (match _derivs_ with
            [] ->
            if true then _conc_
            else errAt _p_ "Wrong rule application: E-Int"
          | _ -> errAt _p_ "The number of premises is wrong: E-Int")
       | _ -> errAt _p_ "The form of conclusion is wrong: E-Int")
| {conc = _conc_; by = "E-Bool"; since = _derivs_; pos = _p_} ->
      (match _conc_ with
         EvalTo(Exp_of_Boolean b, Value_of_Boolean _b) when _b = b && true ->
         (match _derivs_ with
            [] ->
            if true then _conc_
            else errAt _p_ "Wrong rule application: E-Bool"
          | _ -> errAt _p_ "The number of premises is wrong: E-Bool")
       | _ -> errAt _p_ "The form of conclusion is wrong: E-Bool")
| {conc = _conc_; by = "E-IfT"; since = _derivs_; pos = _p_} ->
      (match _conc_ with
         EvalTo(If(e1, e2, e3), v) when true ->
         (match _derivs_ with
            [_d1_; _d2_; ] ->
            (match deriv_check _d1_ with
                 EvalTo(_e1, Value_of_Boolean True) when true ->
                 (match deriv_check _d2_ with
                      EvalTo(__e2, __v) when true ->
                      if _e1 = e1 && __v = v && __e2 = e2 && true then _conc_
                      else errAt _p_ "Wrong rule application: E-IfT"
                  | _ -> errAt _p_ "The form of premise is wrong: E-IfT")
             | _ -> errAt _p_ "The form of premise is wrong: E-IfT")
          | _ -> errAt _p_ "The number of premises is wrong: E-IfT")
       | _ -> errAt _p_ "The form of conclusion is wrong: E-IfT")
| {conc = _conc_; by = "E-IfF"; since = _derivs_; pos = _p_} ->
      (match _conc_ with
         EvalTo(If(e1, e2, e3), v) when true ->
         (match _derivs_ with
            [_d1_; _d2_; ] ->
            (match deriv_check _d1_ with
                 EvalTo(_e1, Value_of_Boolean False) when true ->
                 (match deriv_check _d2_ with
                      EvalTo(__e3, __v) when true ->
                      if _e1 = e1 && __v = v && __e3 = e3 && true then _conc_
                      else errAt _p_ "Wrong rule application: E-IfF"
                  | _ -> errAt _p_ "The form of premise is wrong: E-IfF")
             | _ -> errAt _p_ "The form of premise is wrong: E-IfF")
          | _ -> errAt _p_ "The number of premises is wrong: E-IfF")
       | _ -> errAt _p_ "The form of conclusion is wrong: E-IfF")
| {conc = _conc_; by = "E-Plus"; since = _derivs_; pos = _p_} ->
      (match _conc_ with
         EvalTo(BinOp(Plus, e1, e2), Value_of_int i) when true ->
         (match _derivs_ with
            [_d1_; _d2_; _d3_; ] ->
            (match deriv_check _d1_ with
                 EvalTo(_e1, Value_of_int _i1) when true ->
                 (match deriv_check _d2_ with
                      EvalTo(__e2, Value_of_int __i2) when true ->
                      (match deriv_check _d3_ with
                           AppBOp(Plus, Value_of_int ___i1,
                                  Value_of_int ___i2, Value_of_int ___i3)
                         when true ->
                           if _e1 = e1 && __e2 = e2 && ___i1 = _i1 &&
                              ___i2 = __i2 && true
                           then _conc_
                           else errAt _p_ "Wrong rule application: E-Plus"
                       | _ -> errAt _p_ "The form of premise is wrong: E-Plus")
                  | _ -> errAt _p_ "The form of premise is wrong: E-Plus")
             | _ -> errAt _p_ "The form of premise is wrong: E-Plus")
          | _ -> errAt _p_ "The number of premises is wrong: E-Plus")
       | _ -> errAt _p_ "The form of conclusion is wrong: E-Plus")
| {conc = _conc_; by = "E-Minus"; since = _derivs_; pos = _p_} ->
      (match _conc_ with
         EvalTo(BinOp(Minus, e1, e2), Value_of_int i) when true ->
         (match _derivs_ with
            [_d1_; _d2_; _d3_; ] ->
            (match deriv_check _d1_ with
                 EvalTo(_e1, Value_of_int _i1) when true ->
                 (match deriv_check _d2_ with
                      EvalTo(__e2, Value_of_int __i2) when true ->
                      (match deriv_check _d3_ with
                           AppBOp(Minus, Value_of_int ___i1,
                                  Value_of_int ___i2, Value_of_int ___i3)
                         when true ->
                           if _e1 = e1 && __e2 = e2 && ___i1 = _i1 &&
                              ___i2 = __i2 && true
                           then _conc_
                           else errAt _p_ "Wrong rule application: E-Minus"
                       | _ -> errAt _p_ "The form of premise is wrong: E-Minus")
                  | _ -> errAt _p_ "The form of premise is wrong: E-Minus")
             | _ -> errAt _p_ "The form of premise is wrong: E-Minus")
          | _ -> errAt _p_ "The number of premises is wrong: E-Minus")
       | _ -> errAt _p_ "The form of conclusion is wrong: E-Minus")
| {conc = _conc_; by = "E-Mult"; since = _derivs_; pos = _p_} ->
      (match _conc_ with
         EvalTo(BinOp(Mult, e1, e2), Value_of_int i) when true ->
         (match _derivs_ with
            [_d1_; _d2_; _d3_; ] ->
            (match deriv_check _d1_ with
                 EvalTo(_e1, Value_of_int _i1) when true ->
                 (match deriv_check _d2_ with
                      EvalTo(__e2, Value_of_int __i2) when true ->
                      (match deriv_check _d3_ with
                           AppBOp(Mult, Value_of_int ___i1,
                                  Value_of_int ___i2, Value_of_int ___i3)
                         when true ->
                           if _e1 = e1 && __e2 = e2 && ___i1 = _i1 &&
                              ___i2 = __i2 && true
                           then _conc_
                           else errAt _p_ "Wrong rule application: E-Mult"
                       | _ -> errAt _p_ "The form of premise is wrong: E-Mult")
                  | _ -> errAt _p_ "The form of premise is wrong: E-Mult")
             | _ -> errAt _p_ "The form of premise is wrong: E-Mult")
          | _ -> errAt _p_ "The number of premises is wrong: E-Mult")
       | _ -> errAt _p_ "The form of conclusion is wrong: E-Mult")
| {conc = _conc_; by = "E-Lt"; since = _derivs_; pos = _p_} ->
      (match _conc_ with
         EvalTo(BinOp(Lt, e1, e2), Value_of_Boolean b) when true ->
         (match _derivs_ with
            [_d1_; _d2_; _d3_; ] ->
            (match deriv_check _d1_ with
                 EvalTo(_e1, Value_of_int _i1) when true ->
                 (match deriv_check _d2_ with
                      EvalTo(__e2, Value_of_int __i2) when true ->
                      (match deriv_check _d3_ with
                           AppBOp(Lt, Value_of_int ___i1, Value_of_int ___i2,
                                  Value_of_Boolean ___b3)
                         when true ->
                           if _e1 = e1 && __e2 = e2 && ___i1 = _i1 &&
                              ___i2 = __i2 && true
                           then _conc_
                           else errAt _p_ "Wrong rule application: E-Lt"
                       | _ -> errAt _p_ "The form of premise is wrong: E-Lt")
                  | _ -> errAt _p_ "The form of premise is wrong: E-Lt")
             | _ -> errAt _p_ "The form of premise is wrong: E-Lt")
          | _ -> errAt _p_ "The number of premises is wrong: E-Lt")
       | _ -> errAt _p_ "The form of conclusion is wrong: E-Lt")
| {conc = _conc_; by = "B-Plus"; since = _derivs_; pos = _p_} ->
      (match _conc_ with
         AppBOp(Plus, Value_of_int i1, Value_of_int i2, Value_of_int i3)
       when true ->
         (match _derivs_ with
            [] ->
            if i3 = i1 + i2
            then if true then _conc_
                 else errAt _p_ "Wrong rule application: B-Plus"
            else errAt _p_ "Wrong rule application: B-Plus"
          | _ -> errAt _p_ "The number of premises is wrong: B-Plus")
       | _ -> errAt _p_ "The form of conclusion is wrong: B-Plus")
| {conc = _conc_; by = "B-Minus"; since = _derivs_; pos = _p_} ->
      (match _conc_ with
         AppBOp(Minus, Value_of_int i1, Value_of_int i2, Value_of_int i3)
       when true ->
         (match _derivs_ with
            [] ->
            if i3 = i1 - i2
            then if true then _conc_
                 else errAt _p_ "Wrong rule application: B-Minus"
            else errAt _p_ "Wrong rule application: B-Minus"
          | _ -> errAt _p_ "The number of premises is wrong: B-Minus")
       | _ -> errAt _p_ "The form of conclusion is wrong: B-Minus")
| {conc = _conc_; by = "B-Mult"; since = _derivs_; pos = _p_} ->
      (match _conc_ with
         AppBOp(Mult, Value_of_int i1, Value_of_int i2, Value_of_int i3)
       when true ->
         (match _derivs_ with
            [] ->
            if i3 = i1 * i2
            then if true then _conc_
                 else errAt _p_ "Wrong rule application: B-Mult"
            else errAt _p_ "Wrong rule application: B-Mult"
          | _ -> errAt _p_ "The number of premises is wrong: B-Mult")
       | _ -> errAt _p_ "The form of conclusion is wrong: B-Mult")
| {conc = _conc_; by = "B-Lt"; since = _derivs_; pos = _p_} ->
      (match _conc_ with
         AppBOp(Lt, Value_of_int i1, Value_of_int i2, Value_of_Boolean b3)
       when true ->
         (match _derivs_ with
            [] ->
            if b3 = (if i1 < i2 then True else False)
            then if true then _conc_
                 else errAt _p_ "Wrong rule application: B-Lt"
            else errAt _p_ "Wrong rule application: B-Lt"
          | _ -> errAt _p_ "The number of premises is wrong: B-Lt")
       | _ -> errAt _p_ "The form of conclusion is wrong: B-Lt")
| {by=_name_; pos=_p_} -> errAt _p_ ("No such rule: " ^ _name_)
