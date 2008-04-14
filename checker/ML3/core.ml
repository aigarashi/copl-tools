(* int, Boolean, string, Value, Env, Exp, Prim, i: int, b: Boolean,
   x: string, y: string, v: Value, env: Env, e: Exp, p: Prim,
   True: Boolean[], False: Boolean[], int <: Value, Boolean <: Value,
   Fun: Value[Env, string, Exp], Empty: Env[], Bind: Env[Env, string, Value],
   int <: Exp, Boolean <: Exp, string <: Exp, BinOp: Exp[Prim, Exp, Exp],
   If: Exp[Exp, Exp, Exp], Let: Exp[string, Exp, Exp], Abs: Exp[string, Exp],
   App: Exp[Exp, Exp], Plus: Prim[], Minus: Prim[], Mult: Prim[], Lt: Prim[
   ], EvalTo: j[Env, Exp; Value], AppBOp: j[Prim, Value, Value; Value] *)

open MySupport.Error
open MySupport.Pervasives
open Derivation
type boolean = True | False
and value = Value_of_int of int | Value_of_Boolean of boolean | Fun of env
          * string * exp
and env = Empty | Bind of env * string * value
and exp = Exp_of_int of int | Exp_of_Boolean of boolean
        | Exp_of_string of string | BinOp of prim * exp * exp | If of exp
        * exp * exp | Let of string * exp * exp | Abs of string * exp
        | App of exp * exp
and prim = Plus | Minus | Mult | Lt

type judgment = EvalTo of env * exp * value
              | AppBOp of prim * value * value * value


let rec check_deriv = function
| {conc = _conc_; by = "E-Int"; since = _derivs_; pos = _p_} ->
      (match _conc_ with
         EvalTo(env, Exp_of_int i, Value_of_int _i) when _i = i && true ->
         (match _derivs_ with
            [] ->
            if true then _conc_
            else errAt _p_ "Wrong rule application: E-Int"
          | _ -> errAt _p_ "The number of premises is wrong: E-Int")
       | _ -> errAt _p_ "The form of conclusion is wrong: E-Int")
| {conc = _conc_; by = "E-Bool"; since = _derivs_; pos = _p_} ->
      (match _conc_ with
         EvalTo(env, Exp_of_Boolean b, Value_of_Boolean _b)
       when _b = b && true ->
         (match _derivs_ with
            [] ->
            if true then _conc_
            else errAt _p_ "Wrong rule application: E-Bool"
          | _ -> errAt _p_ "The number of premises is wrong: E-Bool")
       | _ -> errAt _p_ "The form of conclusion is wrong: E-Bool")
| {conc = _conc_; by = "E-IfT"; since = _derivs_; pos = _p_} ->
      (match _conc_ with
         EvalTo(env, If(e1, e2, e3), v) when true ->
         (match _derivs_ with
            [_d1_; _d2_; ] ->
            (match check_deriv _d1_ with
                 EvalTo(_env, _e1, Value_of_Boolean True) when true ->
                 (match check_deriv _d2_ with
                      EvalTo(__env, __e2, __v) when true ->
                      if _e1 = e1 && __v = v && __env = _env &&
                         __env = env && __e2 = e2 && true
                      then _conc_
                      else errAt _p_ "Wrong rule application: E-IfT"
                  | _ -> errAt _p_ "The form of premise is wrong: E-IfT")
             | _ -> errAt _p_ "The form of premise is wrong: E-IfT")
          | _ -> errAt _p_ "The number of premises is wrong: E-IfT")
       | _ -> errAt _p_ "The form of conclusion is wrong: E-IfT")
| {conc = _conc_; by = "E-IfF"; since = _derivs_; pos = _p_} ->
      (match _conc_ with
         EvalTo(env, If(e1, e2, e3), v) when true ->
         (match _derivs_ with
            [_d1_; _d2_; ] ->
            (match check_deriv _d1_ with
                 EvalTo(_env, _e1, Value_of_Boolean False) when true ->
                 (match check_deriv _d2_ with
                      EvalTo(__env, __e3, __v) when true ->
                      if _e1 = e1 && __v = v && __env = _env &&
                         __env = env && __e3 = e3 && true
                      then _conc_
                      else errAt _p_ "Wrong rule application: E-IfF"
                  | _ -> errAt _p_ "The form of premise is wrong: E-IfF")
             | _ -> errAt _p_ "The form of premise is wrong: E-IfF")
          | _ -> errAt _p_ "The number of premises is wrong: E-IfF")
       | _ -> errAt _p_ "The form of conclusion is wrong: E-IfF")
| {conc = _conc_; by = "E-Plus"; since = _derivs_; pos = _p_} ->
      (match _conc_ with
         EvalTo(env, BinOp(Plus, e1, e2), Value_of_int i3) when true ->
         (match _derivs_ with
            [_d1_; _d2_; _d3_; ] ->
            (match check_deriv _d1_ with
                 EvalTo(_env, _e1, Value_of_int _i1) when true ->
                 (match check_deriv _d2_ with
                      EvalTo(__env, __e2, Value_of_int __i2) when true ->
                      (match check_deriv _d3_ with
                           AppBOp(Plus, Value_of_int ___i1,
                                  Value_of_int ___i2, Value_of_int ___i3)
                         when true ->
                           if _e1 = e1 && __env = _env && __env = env &&
                              __e2 = e2 && ___i1 = _i1 && ___i2 = __i2 &&
                              ___i3 = i3 && true
                           then _conc_
                           else errAt _p_ "Wrong rule application: E-Plus"
                       | _ -> errAt _p_ "The form of premise is wrong: E-Plus")
                  | _ -> errAt _p_ "The form of premise is wrong: E-Plus")
             | _ -> errAt _p_ "The form of premise is wrong: E-Plus")
          | _ -> errAt _p_ "The number of premises is wrong: E-Plus")
       | _ -> errAt _p_ "The form of conclusion is wrong: E-Plus")
| {conc = _conc_; by = "E-Minus"; since = _derivs_; pos = _p_} ->
      (match _conc_ with
         EvalTo(env, BinOp(Minus, e1, e2), Value_of_int i3) when true ->
         (match _derivs_ with
            [_d1_; _d2_; _d3_; ] ->
            (match check_deriv _d1_ with
                 EvalTo(_env, _e1, Value_of_int _i1) when true ->
                 (match check_deriv _d2_ with
                      EvalTo(__env, __e2, Value_of_int __i2) when true ->
                      (match check_deriv _d3_ with
                           AppBOp(Minus, Value_of_int ___i1,
                                  Value_of_int ___i2, Value_of_int ___i3)
                         when true ->
                           if _e1 = e1 && __env = _env && __env = env &&
                              __e2 = e2 && ___i1 = _i1 && ___i2 = __i2 &&
                              ___i3 = i3 && true
                           then _conc_
                           else errAt _p_ "Wrong rule application: E-Minus"
                       | _ -> errAt _p_ "The form of premise is wrong: E-Minus")
                  | _ -> errAt _p_ "The form of premise is wrong: E-Minus")
             | _ -> errAt _p_ "The form of premise is wrong: E-Minus")
          | _ -> errAt _p_ "The number of premises is wrong: E-Minus")
       | _ -> errAt _p_ "The form of conclusion is wrong: E-Minus")
| {conc = _conc_; by = "E-Mult"; since = _derivs_; pos = _p_} ->
      (match _conc_ with
         EvalTo(env, BinOp(Mult, e1, e2), Value_of_int i3) when true ->
         (match _derivs_ with
            [_d1_; _d2_; _d3_; ] ->
            (match check_deriv _d1_ with
                 EvalTo(_env, _e1, Value_of_int _i1) when true ->
                 (match check_deriv _d2_ with
                      EvalTo(__env, __e2, Value_of_int __i2) when true ->
                      (match check_deriv _d3_ with
                           AppBOp(Mult, Value_of_int ___i1,
                                  Value_of_int ___i2, Value_of_int ___i3)
                         when true ->
                           if _e1 = e1 && __env = _env && __env = env &&
                              __e2 = e2 && ___i1 = _i1 && ___i2 = __i2 &&
                              ___i3 = i3 && true
                           then _conc_
                           else errAt _p_ "Wrong rule application: E-Mult"
                       | _ -> errAt _p_ "The form of premise is wrong: E-Mult")
                  | _ -> errAt _p_ "The form of premise is wrong: E-Mult")
             | _ -> errAt _p_ "The form of premise is wrong: E-Mult")
          | _ -> errAt _p_ "The number of premises is wrong: E-Mult")
       | _ -> errAt _p_ "The form of conclusion is wrong: E-Mult")
| {conc = _conc_; by = "E-Lt"; since = _derivs_; pos = _p_} ->
      (match _conc_ with
         EvalTo(env, BinOp(Lt, e1, e2), Value_of_Boolean b3) when true ->
         (match _derivs_ with
            [_d1_; _d2_; _d3_; ] ->
            (match check_deriv _d1_ with
                 EvalTo(_env, _e1, Value_of_int _i1) when true ->
                 (match check_deriv _d2_ with
                      EvalTo(__env, __e2, Value_of_int __i2) when true ->
                      (match check_deriv _d3_ with
                           AppBOp(Lt, Value_of_int ___i1, Value_of_int ___i2,
                                  Value_of_Boolean ___b3)
                         when true ->
                           if ___b3 = b3 && _e1 = e1 && __env = _env &&
                              __env = env && __e2 = e2 && ___i1 = _i1 &&
                              ___i2 = __i2 && true
                           then _conc_
                           else errAt _p_ "Wrong rule application: E-Lt"
                       | _ -> errAt _p_ "The form of premise is wrong: E-Lt")
                  | _ -> errAt _p_ "The form of premise is wrong: E-Lt")
             | _ -> errAt _p_ "The form of premise is wrong: E-Lt")
          | _ -> errAt _p_ "The number of premises is wrong: E-Lt")
       | _ -> errAt _p_ "The form of conclusion is wrong: E-Lt")
| {conc = _conc_; by = "E-Var1"; since = _derivs_; pos = _p_} ->
      (match _conc_ with
         EvalTo(Bind(env, x, v), Exp_of_string _x, _v)
       when _v = v && _x = x && true ->
         (match _derivs_ with
            [] ->
            if true then _conc_
            else errAt _p_ "Wrong rule application: E-Var1"
          | _ -> errAt _p_ "The number of premises is wrong: E-Var1")
       | _ -> errAt _p_ "The form of conclusion is wrong: E-Var1")
| {conc = _conc_; by = "E-Var2"; since = _derivs_; pos = _p_} ->
      (match _conc_ with
         EvalTo(Bind(env, y, v1), Exp_of_string x, v2) when true ->
         (match _derivs_ with
            [_d1_; ] ->
            if y <> x
            then (match check_deriv _d1_ with
                      EvalTo(_env, Exp_of_string _x, _v2) when true ->
                      if _env = env && _x = x && _v2 = v2 && true then _conc_
                      else errAt _p_ "Wrong rule application: E-Var2"
                  | _ -> errAt _p_ "The form of premise is wrong: E-Var2")
            else errAt _p_ "Wrong rule application: E-Var2"
          | _ -> errAt _p_ "The number of premises is wrong: E-Var2")
       | _ -> errAt _p_ "The form of conclusion is wrong: E-Var2")
| {conc = _conc_; by = "E-Let"; since = _derivs_; pos = _p_} ->
      (match _conc_ with
         EvalTo(env, Let(x, e1, e2), v) when true ->
         (match _derivs_ with
            [_d1_; _d2_; ] ->
            (match check_deriv _d1_ with
                 EvalTo(_env, _e1, _v1) when true ->
                 (match check_deriv _d2_ with
                      EvalTo(Bind(__env, __x, __v1), __e2, __v) when true ->
                      if _e1 = e1 && __v = v && __env = _env &&
                         __env = env && __e2 = e2 && __x = x && __v1 = _v1 &&
                         true
                      then _conc_
                      else errAt _p_ "Wrong rule application: E-Let"
                  | _ -> errAt _p_ "The form of premise is wrong: E-Let")
             | _ -> errAt _p_ "The form of premise is wrong: E-Let")
          | _ -> errAt _p_ "The number of premises is wrong: E-Let")
       | _ -> errAt _p_ "The form of conclusion is wrong: E-Let")
| {conc = _conc_; by = "E-Fun"; since = _derivs_; pos = _p_} ->
      (match _conc_ with
         EvalTo(env, Abs(x, e), Fun(_env, _x, _e))
       when _e = e && _env = env && _x = x && true ->
         (match _derivs_ with
            [] ->
            if true then _conc_
            else errAt _p_ "Wrong rule application: E-Fun"
          | _ -> errAt _p_ "The number of premises is wrong: E-Fun")
       | _ -> errAt _p_ "The form of conclusion is wrong: E-Fun")
| {conc = _conc_; by = "E-App"; since = _derivs_; pos = _p_} ->
      (match _conc_ with
         EvalTo(env, App(e1, e2), v) when true ->
         (match _derivs_ with
            [_d1_; _d2_; _d3_; ] ->
            (match check_deriv _d1_ with
                 EvalTo(_env, _e1, Fun(_env2, _x, _e0)) when true ->
                 (match check_deriv _d2_ with
                      EvalTo(__env, __e2, __v2) when true ->
                      (match check_deriv _d3_ with
                           EvalTo(Bind(___env2, ___x, ___v2), ___e0, ___v)
                         when true ->
                           if ___env2 = _env2 && ___e0 = _e0 && _e1 = e1 &&
                              ___v = v && __env = _env && __env = env &&
                              __e2 = e2 && ___x = _x && ___v2 = __v2 && true
                           then _conc_
                           else errAt _p_ "Wrong rule application: E-App"
                       | _ -> errAt _p_ "The form of premise is wrong: E-App")
                  | _ -> errAt _p_ "The form of premise is wrong: E-App")
             | _ -> errAt _p_ "The form of premise is wrong: E-App")
          | _ -> errAt _p_ "The number of premises is wrong: E-App")
       | _ -> errAt _p_ "The form of conclusion is wrong: E-App")
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

type in_judgment = In_EvalTo of env * exp | In_AppBOp of prim * value * value
let dummy = Lexing.dummy_pos
let deriv_stack = Stack.create ()

exception NoApplicableRule of in_judgment

let rec make_deriv = function
| In_EvalTo(env, Exp_of_int i) when true && 
       true  -> 
      let _subderivs_ = pop 0 deriv_stack [] in
      (match List.map (fun d -> d.conc) _subderivs_ with
        [] when true ->
        let _conc_ = EvalTo(env, Exp_of_int i, Value_of_int i) in 
        {conc = _conc_; by = "E-Int"; since = _subderivs_; pos = dummy})
| In_EvalTo(env, Exp_of_Boolean b) when true && 
       true  -> 
      let _subderivs_ = pop 0 deriv_stack [] in
      (match List.map (fun d -> d.conc) _subderivs_ with
        [] when true ->
        let _conc_ = EvalTo(env, Exp_of_Boolean b, Value_of_Boolean b) in 
        {conc = _conc_; by = "E-Bool"; since = _subderivs_; pos = dummy})
| In_EvalTo(env, If(e1, e2, e3)) when true && 
      let _d1_ = make_deriv (In_EvalTo(env, e1)) in
      Stack.push _d1_ deriv_stack;
      (match _d1_.conc with
          EvalTo(_, _, Value_of_Boolean True) ->
           let _d2_ = make_deriv (In_EvalTo(env, e2)) in
           Stack.push _d2_ deriv_stack;
           (match _d2_.conc with
               EvalTo(_, _, v) ->
                 true 
             | _ -> for j = 1 to 2 do ignore (Stack.pop deriv_stack) done; false
             )
        | _ -> for j = 1 to 1 do ignore (Stack.pop deriv_stack) done; false
        ) -> 
      let _subderivs_ = pop 2 deriv_stack [] in
      (match List.map (fun d -> d.conc) _subderivs_ with
        [EvalTo(_, _, Value_of_Boolean True); EvalTo(_, _, v)] when true ->
        let _conc_ = EvalTo(env, If(e1, e2, e3), v) in 
        {conc = _conc_; by = "E-IfT"; since = _subderivs_; pos = dummy})
| In_EvalTo(env, If(e1, e2, e3)) when true && 
      let _d1_ = make_deriv (In_EvalTo(env, e1)) in
      Stack.push _d1_ deriv_stack;
      (match _d1_.conc with
          EvalTo(_, _, Value_of_Boolean False) ->
           let _d2_ = make_deriv (In_EvalTo(env, e3)) in
           Stack.push _d2_ deriv_stack;
           (match _d2_.conc with
               EvalTo(_, _, v) ->
                 true 
             | _ -> for j = 1 to 2 do ignore (Stack.pop deriv_stack) done; false
             )
        | _ -> for j = 1 to 1 do ignore (Stack.pop deriv_stack) done; false
        ) -> 
      let _subderivs_ = pop 2 deriv_stack [] in
      (match List.map (fun d -> d.conc) _subderivs_ with
        [EvalTo(_, _, Value_of_Boolean False); EvalTo(_, _, v)] when true ->
        let _conc_ = EvalTo(env, If(e1, e2, e3), v) in 
        {conc = _conc_; by = "E-IfF"; since = _subderivs_; pos = dummy})
| In_EvalTo(env, BinOp(Plus, e1, e2)) when true && 
      let _d1_ = make_deriv (In_EvalTo(env, e1)) in
      Stack.push _d1_ deriv_stack;
      (match _d1_.conc with
          EvalTo(_, _, Value_of_int i1) ->
           let _d2_ = make_deriv (In_EvalTo(env, e2)) in
           Stack.push _d2_ deriv_stack;
           (match _d2_.conc with
               EvalTo(_, _, Value_of_int i2) ->
                let _d3_ = make_deriv (In_AppBOp(Plus, Value_of_int i1,
                                                 Value_of_int i2)) in
                Stack.push _d3_ deriv_stack;
                (match _d3_.conc with
                    AppBOp(_, _, _, Value_of_int i3) ->
                      true 
                  | _ -> for j = 1 to 3 do ignore (Stack.pop deriv_stack) done; false
                  )
             | _ -> for j = 1 to 2 do ignore (Stack.pop deriv_stack) done; false
             )
        | _ -> for j = 1 to 1 do ignore (Stack.pop deriv_stack) done; false
        ) -> 
      let _subderivs_ = pop 3 deriv_stack [] in
      (match List.map (fun d -> d.conc) _subderivs_ with
        [EvalTo(_, _, Value_of_int i1); EvalTo(_, _, Value_of_int i2);
        AppBOp(_, _, _, Value_of_int i3)] when true ->
        let _conc_ = EvalTo(env, BinOp(Plus, e1, e2), Value_of_int i3) in 
        {conc = _conc_; by = "E-Plus"; since = _subderivs_; pos = dummy})
| In_EvalTo(env, BinOp(Minus, e1, e2)) when true && 
      let _d1_ = make_deriv (In_EvalTo(env, e1)) in
      Stack.push _d1_ deriv_stack;
      (match _d1_.conc with
          EvalTo(_, _, Value_of_int i1) ->
           let _d2_ = make_deriv (In_EvalTo(env, e2)) in
           Stack.push _d2_ deriv_stack;
           (match _d2_.conc with
               EvalTo(_, _, Value_of_int i2) ->
                let _d3_ = make_deriv (In_AppBOp(Minus, Value_of_int i1,
                                                 Value_of_int i2)) in
                Stack.push _d3_ deriv_stack;
                (match _d3_.conc with
                    AppBOp(_, _, _, Value_of_int i3) ->
                      true 
                  | _ -> for j = 1 to 3 do ignore (Stack.pop deriv_stack) done; false
                  )
             | _ -> for j = 1 to 2 do ignore (Stack.pop deriv_stack) done; false
             )
        | _ -> for j = 1 to 1 do ignore (Stack.pop deriv_stack) done; false
        ) -> 
      let _subderivs_ = pop 3 deriv_stack [] in
      (match List.map (fun d -> d.conc) _subderivs_ with
        [EvalTo(_, _, Value_of_int i1); EvalTo(_, _, Value_of_int i2);
        AppBOp(_, _, _, Value_of_int i3)] when true ->
        let _conc_ = EvalTo(env, BinOp(Minus, e1, e2), Value_of_int i3) in 
        {conc = _conc_; by = "E-Minus"; since = _subderivs_; pos = dummy})
| In_EvalTo(env, BinOp(Mult, e1, e2)) when true && 
      let _d1_ = make_deriv (In_EvalTo(env, e1)) in
      Stack.push _d1_ deriv_stack;
      (match _d1_.conc with
          EvalTo(_, _, Value_of_int i1) ->
           let _d2_ = make_deriv (In_EvalTo(env, e2)) in
           Stack.push _d2_ deriv_stack;
           (match _d2_.conc with
               EvalTo(_, _, Value_of_int i2) ->
                let _d3_ = make_deriv (In_AppBOp(Mult, Value_of_int i1,
                                                 Value_of_int i2)) in
                Stack.push _d3_ deriv_stack;
                (match _d3_.conc with
                    AppBOp(_, _, _, Value_of_int i3) ->
                      true 
                  | _ -> for j = 1 to 3 do ignore (Stack.pop deriv_stack) done; false
                  )
             | _ -> for j = 1 to 2 do ignore (Stack.pop deriv_stack) done; false
             )
        | _ -> for j = 1 to 1 do ignore (Stack.pop deriv_stack) done; false
        ) -> 
      let _subderivs_ = pop 3 deriv_stack [] in
      (match List.map (fun d -> d.conc) _subderivs_ with
        [EvalTo(_, _, Value_of_int i1); EvalTo(_, _, Value_of_int i2);
        AppBOp(_, _, _, Value_of_int i3)] when true ->
        let _conc_ = EvalTo(env, BinOp(Mult, e1, e2), Value_of_int i3) in 
        {conc = _conc_; by = "E-Mult"; since = _subderivs_; pos = dummy})
| In_EvalTo(env, BinOp(Lt, e1, e2)) when true && 
      let _d1_ = make_deriv (In_EvalTo(env, e1)) in
      Stack.push _d1_ deriv_stack;
      (match _d1_.conc with
          EvalTo(_, _, Value_of_int i1) ->
           let _d2_ = make_deriv (In_EvalTo(env, e2)) in
           Stack.push _d2_ deriv_stack;
           (match _d2_.conc with
               EvalTo(_, _, Value_of_int i2) ->
                let _d3_ = make_deriv (In_AppBOp(Lt, Value_of_int i1,
                                                 Value_of_int i2)) in
                Stack.push _d3_ deriv_stack;
                (match _d3_.conc with
                    AppBOp(_, _, _, Value_of_Boolean b3) ->
                      true 
                  | _ -> for j = 1 to 3 do ignore (Stack.pop deriv_stack) done; false
                  )
             | _ -> for j = 1 to 2 do ignore (Stack.pop deriv_stack) done; false
             )
        | _ -> for j = 1 to 1 do ignore (Stack.pop deriv_stack) done; false
        ) -> 
      let _subderivs_ = pop 3 deriv_stack [] in
      (match List.map (fun d -> d.conc) _subderivs_ with
        [EvalTo(_, _, Value_of_int i1); EvalTo(_, _, Value_of_int i2);
        AppBOp(_, _, _, Value_of_Boolean b3)] when true ->
        let _conc_ = EvalTo(env, BinOp(Lt, e1, e2), Value_of_Boolean b3) in 
        {conc = _conc_; by = "E-Lt"; since = _subderivs_; pos = dummy})
| In_EvalTo(Bind(env, x, v), Exp_of_string _x) when _x = x && true && 
       true  -> 
      let _subderivs_ = pop 0 deriv_stack [] in
      (match List.map (fun d -> d.conc) _subderivs_ with
        [] when _x = x &&
        true ->
        let _conc_ = EvalTo(Bind(env, x, v), Exp_of_string x, v) in 
        {conc = _conc_; by = "E-Var1"; since = _subderivs_; pos = dummy})
| In_EvalTo(Bind(env, y, v1), Exp_of_string x) when true && 
      y <> x && let _d1_ = make_deriv (In_EvalTo(env, Exp_of_string x)) in
      Stack.push _d1_ deriv_stack;
      (match _d1_.conc with
          EvalTo(_, _, v2) ->
            true 
        | _ -> for j = 1 to 1 do ignore (Stack.pop deriv_stack) done; false
        ) -> 
      let _subderivs_ = pop 1 deriv_stack [] in
      (match List.map (fun d -> d.conc) _subderivs_ with
        [EvalTo(_, _, v2)] when true ->
        if not (y <> x) then err "Implementation error!" else
        let _conc_ = EvalTo(Bind(env, y, v1), Exp_of_string x, v2) in 
        {conc = _conc_; by = "E-Var2"; since = _subderivs_; pos = dummy})
| In_EvalTo(env, Let(x, e1, e2)) when true && 
      let _d1_ = make_deriv (In_EvalTo(env, e1)) in
      Stack.push _d1_ deriv_stack;
      (match _d1_.conc with
          EvalTo(_, _, v1) ->
           let _d2_ = make_deriv (In_EvalTo(Bind(env, x, v1), e2)) in
           Stack.push _d2_ deriv_stack;
           (match _d2_.conc with
               EvalTo(_, _, v) ->
                 true 
             | _ -> for j = 1 to 2 do ignore (Stack.pop deriv_stack) done; false
             )
        | _ -> for j = 1 to 1 do ignore (Stack.pop deriv_stack) done; false
        ) -> 
      let _subderivs_ = pop 2 deriv_stack [] in
      (match List.map (fun d -> d.conc) _subderivs_ with
        [EvalTo(_, _, v1); EvalTo(_, _, v)] when true ->
        let _conc_ = EvalTo(env, Let(x, e1, e2), v) in 
        {conc = _conc_; by = "E-Let"; since = _subderivs_; pos = dummy})
| In_EvalTo(env, Abs(x, e)) when true && 
       true  -> 
      let _subderivs_ = pop 0 deriv_stack [] in
      (match List.map (fun d -> d.conc) _subderivs_ with
        [] when true ->
        let _conc_ = EvalTo(env, Abs(x, e), Fun(env, x, e)) in 
        {conc = _conc_; by = "E-Fun"; since = _subderivs_; pos = dummy})
| In_EvalTo(env, App(e1, e2)) when true && 
      let _d1_ = make_deriv (In_EvalTo(env, e1)) in
      Stack.push _d1_ deriv_stack;
      (match _d1_.conc with
          EvalTo(_, _, Fun(env2, x, e0)) ->
           let _d2_ = make_deriv (In_EvalTo(env, e2)) in
           Stack.push _d2_ deriv_stack;
           (match _d2_.conc with
               EvalTo(_, _, v2) ->
                let _d3_ = make_deriv (In_EvalTo(Bind(env2, x, v2), e0)) in
                Stack.push _d3_ deriv_stack;
                (match _d3_.conc with
                    EvalTo(_, _, v) ->
                      true 
                  | _ -> for j = 1 to 3 do ignore (Stack.pop deriv_stack) done; false
                  )
             | _ -> for j = 1 to 2 do ignore (Stack.pop deriv_stack) done; false
             )
        | _ -> for j = 1 to 1 do ignore (Stack.pop deriv_stack) done; false
        ) -> 
      let _subderivs_ = pop 3 deriv_stack [] in
      (match List.map (fun d -> d.conc) _subderivs_ with
        [EvalTo(_, _, Fun(env2, x, e0)); EvalTo(_, _, v2); EvalTo(_, _, v)] when true ->
        let _conc_ = EvalTo(env, App(e1, e2), v) in 
        {conc = _conc_; by = "E-App"; since = _subderivs_; pos = dummy})
| In_AppBOp(Plus, Value_of_int i1, Value_of_int i2) when true && 
      let i3 = i1 + i2 in   true
       -> 
      let _subderivs_ = pop 0 deriv_stack [] in
      (match List.map (fun d -> d.conc) _subderivs_ with
        [] when true ->
        let i3 = i1 + i2 in 
        let _conc_ = AppBOp(Plus, Value_of_int i1, Value_of_int i2,
                            Value_of_int i3) in 
        {conc = _conc_; by = "B-Plus"; since = _subderivs_; pos = dummy})
| In_AppBOp(Minus, Value_of_int i1, Value_of_int i2) when true && 
      let i3 = i1 - i2 in   true
       -> 
      let _subderivs_ = pop 0 deriv_stack [] in
      (match List.map (fun d -> d.conc) _subderivs_ with
        [] when true ->
        let i3 = i1 - i2 in 
        let _conc_ = AppBOp(Minus, Value_of_int i1, Value_of_int i2,
                            Value_of_int i3) in 
        {conc = _conc_; by = "B-Minus"; since = _subderivs_; pos = dummy})
| In_AppBOp(Mult, Value_of_int i1, Value_of_int i2) when true && 
      let i3 = i1 * i2 in   true
       -> 
      let _subderivs_ = pop 0 deriv_stack [] in
      (match List.map (fun d -> d.conc) _subderivs_ with
        [] when true ->
        let i3 = i1 * i2 in 
        let _conc_ = AppBOp(Mult, Value_of_int i1, Value_of_int i2,
                            Value_of_int i3) in 
        {conc = _conc_; by = "B-Mult"; since = _subderivs_; pos = dummy})
| In_AppBOp(Lt, Value_of_int i1, Value_of_int i2) when true && 
      let b3 = (if i1 < i2 then True else False) in   true
       -> 
      let _subderivs_ = pop 0 deriv_stack [] in
      (match List.map (fun d -> d.conc) _subderivs_ with
        [] when true ->
        let b3 = (if i1 < i2 then True else False) in 
        let _conc_ = AppBOp(Lt, Value_of_int i1, Value_of_int i2,
                            Value_of_Boolean b3) in 
        {conc = _conc_; by = "B-Lt"; since = _subderivs_; pos = dummy})
| j -> raise (NoApplicableRule j)