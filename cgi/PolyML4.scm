(defire rulenames
  (list
    "T-Int"
    "T-Bool"
    "T-If"
    "T-Plus"
    "T-Minus"
    "T-Mult"
    "T-Lt"
    "T-Var1"
    "T-Var2"
    "T-Let"
    "T-Abs"
    "T-App"
    "T-LetRec"))

(define rules
  (list
    (infrule
      "T-Int"
      (list)
      (Typing (mv "env" "") (mv "i" "") (TyIntTerm)))
    (infrule
      "T-Bool"
      (list)
      (Typing (mv "env" "") (mv "b" "") (TyBoolTerm)))
    (infrule
      "T-If"
      (list
        (Typing (mv "env" "") (mv "e" "1") (TyBoolTerm))
        (Typing (mv "env" "") (mv "e" "2") (mv "t" ""))
        (Typing (mv "env" "") (mv "e" "3") (mv "t" "")))
      (Typing (mv "env" "") (IfTerm (mv "e" "1") (mv "e" "2") (mv "e" "3"))
                (mv "t" "")))
    (infrule
      "T-Plus"
      (list
        (Typing (mv "env" "") (mv "e" "1") (TyIntTerm))
        (Typing (mv "env" "") (mv "e" "2") (TyIntTerm)))
      (Typing (mv "env" "") (BinOpTerm (PlusTerm) (mv "e" "1") (mv "e" "2"))
                (TyIntTerm)))
    (infrule
      "T-Minus"
      (list
        (Typing (mv "env" "") (mv "e" "1") (TyIntTerm))
        (Typing (mv "env" "") (mv "e" "2") (TyIntTerm)))
      (Typing (mv "env" "") (BinOpTerm (MinusTerm) (mv "e" "1") (mv "e" "2"))
                (TyIntTerm)))
    (infrule
      "T-Mult"
      (list
        (Typing (mv "env" "") (mv "e" "1") (TyIntTerm))
        (Typing (mv "env" "") (mv "e" "2") (TyIntTerm)))
      (Typing (mv "env" "") (BinOpTerm (MultTerm) (mv "e" "1") (mv "e" "2"))
                (TyIntTerm)))
    (infrule
      "T-Lt"
      (list
        (Typing (mv "env" "") (mv "e" "1") (TyIntTerm))
        (Typing (mv "env" "") (mv "e" "2") (TyIntTerm)))
      (Typing (mv "env" "") (BinOpTerm (LtTerm) (mv "e" "1") (mv "e" "2"))
                (TyBoolTerm)))
    (infrule
      "T-Var1"
      (list
        `#"inst ,(mv "s" "") ,(mv "t" "")")
      (Typing (BindTerm (mv "env" "") (mv "x" "") (mv "s" "")) (mv "x" "")
                (mv "t" "")))
    (infrule
      "T-Var2"
      (list
        `#",(mv "y" "") <> ,(mv "x" "")"
        (Typing (mv "env" "") (mv "x" "") (mv "t" "")))
      (Typing (BindTerm (mv "env" "") (mv "y" "") (mv "s" "")) (mv "x" "")
                (mv "t" "")))
    (infrule
      "T-Let"
      (list
        (Typing (mv "env" "") (mv "e" "1") (mv "t" "1"))
        (Typing (BindTerm (mv "env" "") (mv "x" "") (mv "s" "")) (mv "e" "2")
                  (mv "t" "2"))
        `#"closure ,(mv "env" "") ,(mv "t" "1") ,(mv "s" "")")
      (Typing (mv "env" "") (LetTerm (mv "x" "") (mv "e" "1") (mv "e" "2"))
                (mv "t" "2")))
    (infrule
      "T-Abs"
      (list
        (Typing (BindTerm (mv "env" "") (mv "x" "") (mv "t" "1")) (mv "e" "")
                  (mv "t" "2")))
      (Typing (mv "env" "") (AbsTerm (mv "x" "") (mv "e" ""))
                                                             (TyFunTerm
                                                              (mv "t" "1")
                                                              (mv "t" "2"))))
    (infrule
      "T-App"
      (list
        (Typing (mv "env" "") (mv "e" "1")
                                          (TyFunTerm (mv "t" "1")
                                                                 (mv "t" "2")))
        (Typing (mv "env" "") (mv "e" "2") (mv "t" "1")))
      (Typing (mv "env" "") (AppTerm (mv "e" "1") (mv "e" "2")) (mv "t" "2")))
    (infrule
      "T-LetRec"
      (list
        (Typing
               (BindTerm
                        (BindTerm (mv "env" "") (mv "x" "")
                                                           (TyFunTerm
                                                            (mv "t" "1")
                                                            (mv "t" "2")))
                (mv "y" "") (mv "t" "1")) (mv "e" "1")
                                                      (TyFunTerm (mv "t" "1")
                                                       (mv "t" "2")))
        (Typing (BindTerm (mv "env" "") (mv "x" "") (mv "s" "")) (mv "e" "2")
                  (mv "t" ""))
        `#"closure ,(mv "env" "") (TyFun(,(mv "t" "1"),,(mv "t" "2"))) ,(mv "s" "")")
      (Typing (mv "env" "")
                           (LetRecTerm (mv "x" "") (mv "y" "") (mv "e" "1")
                            (mv "e" "2")) (mv "t" "")))))
