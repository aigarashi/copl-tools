#! /usr/bin/gosh
;; translation from inference rules to MathML
;;
;; $Id: $

(use text.html-lite)
(use text.tree)
(use srfi-13)
(use util.list)

(define (normalize-rname rn)
  (string-downcase (string-filter rn char-alphabetic?)))

(define (bnf name mvars terms)
  (let ((delimited-metavars (intersperse ", " mvars))
	(delimited-terms (intersperse " \\mid " terms))
	(nameID (normalize-rname name)))
    (if (null? delimited-terms)
	(list delimited-metavars " \\in " name)
	(list delimited-metavars " \\in " name " \\;\\mbox{::=}\\; " delimited-terms))))

(define (format-bnf bnfdefs)
  (html:div :class "bnf"
     (html:pre 
      :class "TeX"
      (html:div
       "$\\displaystyle{\\begin{array}{l}\n"
       (intersperse " \\\\ " bnfdefs)
       "\\end{array}}$\n"))))

(define (infrule name premises concl)
  (let ((delimited-premises (intersperse " \\qquad " premises))
	(nameID (normalize-rname name)))
    (html:div 
     :id nameID :class "rule" ; :style "display: none;"
     (html:pre 
      :class "TeX"
      (html:div
       `("$\\displaystyle{\\frac{\n"
	 ,delimited-premises "}{\n"
	 ,concl "}}$"
	 ,(html:span :class "rname" "(" name ")")))))))

(define (mv base suffix alist) 
  ;; formatting metavariables
  ;; alist is to transform names to special symbols
  (let* ((tmp (assoc base alist))
	 (base (if tmp (cadr tmp) base)))
    (if suffix
	#`"\\textcolor{brown}{\\mathbf{,|base|_,suffix}}"
	#`"\\textcolor{brown}{\\mathbf{,base}}")))

;;;; HTML stuff
(define (header-LaTeXMathML) ; for loading LaTeXMathML
  (list (html:script :type "text/javascript" 
;		     :src "http://math.etsu.edu/LaTeXMathML/LaTeXMathML.js")
		     :src "../LaTeXMathML.js")
	(html:link :rel "stylesheet" :type "text/css" 
;		   :href "http://math.etsu.edu/LaTeXMathML/LaTeXMathML.standardarticle.css")))
		   :href "../LaTeXMathML.standardarticle.css")))

(define (rule-style) ; style of rules
  (html:style :type "text/css" "
div.rule  { text-align: center; }
div.bnf  { text-align: center; }
span.rname { font-variant: small-caps; }
"))

(define (js:ShowStuff)
  ;; Javascipt function to swap display/no display for all content
  ;; within span tags Click_Menu  
(html:script :language "JavaScript" 
"<!--
 var current=\"\"; // no rule is displayed at first

 function Show_Stuff(Click_Menu) {
 // Function that will swap the display/no display for
 // all content within span/div tags

 if (Click_Menu.style.display == \"none\") {
   if (current != \"\") current.style.display = \"none\"; 
   Click_Menu.style.display = \"\";
   current = Click_Menu;
 } else {
   Click_Menu.style.display = \"none\";
   current = \"\";
 }
 }
 -->"))

(define (rnameref rn) ; link to call Show_Stuff
  (html:a :href #`"javascript:Show_Stuff(,(normalize-rname rn))" rn))

;;; main function
(define (main args)
  (load (string-append "./" (cadr args) ".scm"))
  (write-tree 
   (list
    (html-doctype)
    (html:html
     (html:head 
      (rule-style)
      (header-LaTeXMathML))
     (html:body
      (format-bnf bnfdefs)
      rules))))
  0)

;;; game specific functions follow

;; Game nat
(define (Nat:mv base . suffix)
  (mv base (and (pair? suffix) (car suffix)) '()))

(define (Nat:STerm n)
  `("S(" ,n ")"))

(define (Nat:ZTerm) "Z")

(define (Nat:PlusIs n1 n2 n3)
  `(,n1 "\\mbox{ plus }" ,n2 "\\mbox{ is }" ,n3))

(define (Nat:MultIs n1 n2 n3)
  `(,n1 "\\mbox{ times }" ,n2 "\\mbox{ is }" ,n3))

;; Game CompareNat[1-3]
(define CompareNat1:mv Nat:mv)
(define CompareNat1:STerm Nat:STerm)
(define CompareNat1:ZTerm Nat:ZTerm)
(define (CompareNat1:Lt n1 n2)
  `(,n1 "\\mbox{ is less than }" ,n2))

(define CompareNat2:mv Nat:mv)
(define CompareNat2:STerm Nat:STerm)
(define CompareNat2:ZTerm Nat:ZTerm)
(define CompareNat2:Lt CompareNat1:Lt)

(define CompareNat3:mv Nat:mv)
(define CompareNat3:STerm Nat:STerm)
(define CompareNat3:ZTerm Nat:ZTerm)
(define CompareNat3:Lt CompareNat1:Lt)

;; Game EvalNatExp
(define EvalNatExp:mv Nat:mv)
(define EvalNatExp:STerm Nat:STerm)
(define EvalNatExp:ZTerm Nat:ZTerm)
(define EvalNatExp:PlusIs Nat:PlusIs)
(define EvalNatExp:MultIs Nat:MultIs)
(define (EvalNatExp:PTerm e1 e2)
  `(,e1 "+" ,e2))
(define (EvalNatExp:MTerm e1 e2)
  `(,e1 "*" ,e2))
(define (EvalNatExp:EvalTo e n)
  `(,e "\\Downarrow" ,n))

;; Game ReduceNatExp
(define ReduceNatExp:mv Nat:mv)
(define ReduceNatExp:STerm Nat:STerm)
(define ReduceNatExp:ZTerm Nat:ZTerm)
(define ReduceNatExp:PlusIs Nat:PlusIs)
(define ReduceNatExp:MultIs Nat:MultIs)
(define (ReduceNatExp:PTerm e1 e2)
  `(,e1 "+" ,e2))
(define (ReduceNatExp:MTerm e1 e2)
  `(,e1 "*" ,e2))
(define (ReduceNatExp:OneStepTo e1 e2)
  `(,e1 "\\longrightarrow" ,e2))
(define (ReduceNatExp:MultiStepTo e1 e2)
  `(,e1 "\\stackrel{*}{\\longrightarrow}" ,e2))
(define (ReduceNatExp:OneStepToD e1 e2)
  `(,e1 "\\stackrel{d}{\\longrightarrow}" ,e2))

;; ML1
(define (ML1:mv base . suffix)
  (mv base (and (pair? suffix) (car suffix)) '()))

(define (ML1:BinOpTerm p e1 e2)
  `(,e1 "\\," ,p "\\," ,e2))

(define (ML1:IfTerm e1 e2 e3)
  `("\\mbox{if }" ,e1 "\\mbox{ then }" ,e2 "\\mbox{ else }" ,e3))

(define (ML1:PlusTerm) '+)
(define (ML1:MinusTerm) '-)
(define (ML1:MultTerm) '*)
(define (ML1:LtTerm) '<)

(define (ML1:EvalTo e v)
  `(,e "\\Downarrow" ,v))

(define (ML1:AppBOp p v1 v2 v3)
  (let ((p (cadr (assq p '((+ "\\mbox{ plus }")
			   (- "\\mbox{ minus }")
			   (* "\\mbox{ times }")
			   (< "\\mbox{ less than }"))))))
    `(,v1 ,p ,v2 "\\mbox{ is }" ,v3)))

;; ContML1
(define ContML1:mv ML1:mv)

(define ContML1:BinOpTerm ML1:BinOpTerm)
(define ContML1:IfTerm ML1:IfTerm)

(define (ContML1:RetKTerm) '("\\mbox{_}"))
(define (ContML1:EvalRKTerm e op k)
  `("\\{" ,@(ML1:BinOpTerm op "\\mbox{_}" e) "\\} \\gg " ,k))
(define (ContML1:AppOpKTerm v op k)
  `("\\{" ,@(ML1:BinOpTerm op v "\\mbox{_}") "\\} \\gg " ,k))
(define (ContML1:BranchKTerm e1 e2 k)
  `("\\{" "\\mbox{if _ then }" ,e1 "\\mbox{ else }" ,e2 "\\} \\gg" ,k))

(define ContML1:PlusTerm ML1:PlusTerm)
(define ContML1:MinusTerm ML1:MinusTerm)
(define ContML1:MultTerm ML1:MultTerm)
(define ContML1:LtTerm ML1:LtTerm)

(define (ContML1:EvalTo k e v)
  `(,e " \\gg " ,k "\\Downarrow" ,v))
(define ContML1:AppBOp ML1:AppBOp)
(define (ContML1:AppK k v1 v2)
  `(,v1 "\\Rightarrow" ,k "\\Downarrow" ,v2))

;; ML2
(define (ML2:mv base . suffix)
  (mv base (and (pair? suffix) (car suffix)) '(("env" "\\mathcal{E}"))))

(define (ML2:EmptyTerm) "\\cdot")

(define (ML2:BindTerm env x v)
  (if (equal? env "\\cdot")
      `(,x "=" ,v)
      `(,env "," ,x "=" ,v)))

(define ML2:BinOpTerm ML1:BinOpTerm)
(define ML2:IfTerm ML1:IfTerm)

(define (ML2:LetTerm x e1 e2)
  `("\\mbox{let }" ,x " = " ,e1 "\\mbox{ in }" ,e2))

(define ML2:PlusTerm ML1:PlusTerm)
(define ML2:MinusTerm ML1:MinusTerm)
(define ML2:MultTerm ML1:MultTerm)
(define ML2:LtTerm ML1:LtTerm)

(define (ML2:EvalTo env e v)
  `(,env "\\vdash" ,@(ML1:EvalTo e v)))

(define ML2:AppBOp ML1:AppBOp)

;; ML3
(define ML3:mv ML2:mv)

(define (ML3:FunTerm env x e)
  `("\\mbox{(}" ,env "\\mbox{)[fun }" ,x "\\rightarrow" ,e "\\mbox{]}"))

(define ML3:EmptyTerm ML2:EmptyTerm)
(define ML3:BindTerm ML2:BindTerm)
(define ML3:BinOpTerm ML2:BinOpTerm)
(define ML3:IfTerm ML2:IfTerm)
(define ML3:LetTerm ML2:LetTerm)

(define (ML3:AbsTerm x e)
  `("\\mbox{fun }" ,x "\\rightarrow" ,e))

(define (ML3:AppTerm e1 e2)
  `(,e1 "\\;" ,e2))

(define ML3:PlusTerm ML2:PlusTerm)
(define ML3:MinusTerm ML2:MinusTerm)
(define ML3:MultTerm ML2:MultTerm)
(define ML3:LtTerm ML2:LtTerm)

(define ML3:EvalTo ML2:EvalTo)

(define ML3:AppBOp ML2:AppBOp)

;; ML4
(define ML4:mv ML3:mv)

(define ML4:FunTerm ML3:FunTerm)
(define (ML4:RecTerm env x y e)
  `("\\mbox{(}" ,env 
    "\\mbox{)[rec }" ,x " = \\mbox{fun }" ,y "\\rightarrow" ,e "\\mbox{]}"))

(define ML4:EmptyTerm ML3:EmptyTerm)
(define ML4:BindTerm ML3:BindTerm)
(define ML4:BinOpTerm ML3:BinOpTerm)
(define ML4:IfTerm ML3:IfTerm)
(define ML4:LetTerm ML3:LetTerm)
(define ML4:AbsTerm ML3:AbsTerm)
(define ML4:AppTerm ML3:AppTerm)

(define (ML4:LetRecTerm x y e1 e2)
    `("\\mbox{let rec }" ,x " = \\mbox{fun }" ,y " \\rightarrow " ,e1 "\\mbox{ in }" ,e2))

(define ML4:PlusTerm ML3:PlusTerm)
(define ML4:MinusTerm ML3:MinusTerm)
(define ML4:MultTerm ML3:MultTerm)
(define ML4:LtTerm ML3:LtTerm)

(define ML4:EvalTo ML3:EvalTo)

(define ML4:AppBOp ML3:AppBOp)

;; ContML4
(define ContML4:mv ML4:mv)

(define ContML4:FunTerm ML4:FunTerm)
(define ContML4:RecTerm ML4:RecTerm)
(define (ContML4:ContFTerm k)
  `("[" ,k "]"))

(define ContML4:EmptyTerm ML4:EmptyTerm)
(define ContML4:BindTerm ML4:BindTerm)
(define ContML4:BinOpTerm ML4:BinOpTerm)
(define ContML4:IfTerm ML4:IfTerm)
(define ContML4:LetTerm ML4:LetTerm)
(define ContML4:AbsTerm ML4:AbsTerm)
(define ContML4:AppTerm ML4:AppTerm)
(define ContML4:LetRecTerm ML4:LetRecTerm)
(define (ContML4:LetCcTerm x e)
  `("\\mbox{letcc }" ,x "\\mbox{ in }" ,e))

(define (ContML4:RetKTerm) '("\\mbox{_}"))
(define (ContML4:EvalRKTerm env e op k)
  `("\\{" ,env "\\vdash " ,@(ML1:BinOpTerm op "\\mbox{_}" e) "\\} \\gg " ,k))
(define (ContML4:AppOpKTerm v op k)
  `("\\{" ,@(ML1:BinOpTerm op v "\\mbox{_}") "\\} \\gg " ,k))
(define (ContML4:BranchKTerm env e1 e2 k)
  `("\\{" ,env "\\vdash \\mbox{if _ then }" ,e1 "\\mbox{ else }" ,e2 "\\} \\gg" ,k))
(define (ContML4:LetBodyKTerm env x e k)
  `("\\{" ,env "\\vdash \\mbox{let }" ,x "= \\mbox{_} \\mbox{ in } " ,e "\\} \\gg k"))
(define (ContML4:EvalArgKTerm env e k)
  `("\\{" ,env "\\vdash \\mbox{_}\\, " ,e "\\} \\gg" ,k))
(define (ContML4:AppFunKTerm v k)
  `("\\{" ,v "\\,\\mbox{_}\\} \\gg " ,k))

(define ContML4:PlusTerm ML4:PlusTerm)
(define ContML4:MinusTerm ML4:MinusTerm)
(define ContML4:MultTerm ML4:MultTerm)
(define ContML4:LtTerm ML4:LtTerm)

(define (ContML4:EvalTo env k e v)
  `(,env "\\vdash" ,e " \\gg " ,k "\\Downarrow" ,v))
(define ContML4:AppBOp ML4:AppBOp)
(define (ContML4:AppK k v1 v2)
  `(,v1 "\\Rightarrow" ,k "\\Downarrow" ,v2))

;; RefML4
(define RefML4:mv ML4:mv)

(define RefML4:FunTerm ML4:FunTerm)
(define RefML4:RecTerm ML4:RecTerm)

(define RefML4:EmptyTerm ML4:EmptyTerm)
(define RefML4:BindTerm ML4:BindTerm)

(define (RefML4:EmptySTerm) "\\cdot")

(define (RefML4:BlockTerm env x v)
  (if (equal? env "\\cdot")
      `(,x "=" ,v)
      `(,env ", " ,x "=" ,v)))

(define RefML4:BinOpTerm ML4:BinOpTerm)
(define RefML4:IfTerm ML4:IfTerm)
(define RefML4:LetTerm ML4:LetTerm)
(define RefML4:AbsTerm ML4:AbsTerm)
(define RefML4:AppTerm ML4:AppTerm)
(define RefML4:LetRecTerm ML4:LetRecTerm)
(define (RefML4:NewRefTerm e)
  `("ref " ,e))
(define (RefML4:DerefTerm e)
  `("!" ,e))
(define (RefML4:AssignTerm e1 e2)
  `(,e1 " := " ,e2))

(define RefML4:PlusTerm ML4:PlusTerm)
(define RefML4:MinusTerm ML4:MinusTerm)
(define RefML4:MultTerm ML4:MultTerm)
(define RefML4:LtTerm ML4:LtTerm)

(define (RefML4:EvalTo s1 env e v s2)
  `(,s1 " / " ,@(ML4:EvalTo env e v) " / " ,s2))

(define RefML4:AppBOp ML4:AppBOp)

;; ML5
(define ML5:mv ML4:mv)

(define ML5:FunTerm ML4:FunTerm)
(define ML5:RecTerm ML4:RecTerm)
(define (ML5:NilVTerm) "[]")
(define (ML5:ConsVTerm v1 v2) 
  `(,v1 "\\mbox{ :: }" ,v2))

(define ML5:EmptyTerm ML4:EmptyTerm)
(define ML5:BindTerm ML4:BindTerm)
(define ML5:BinOpTerm ML4:BinOpTerm)
(define ML5:IfTerm ML4:IfTerm)
(define ML5:LetTerm ML4:LetTerm)
(define ML5:AbsTerm ML4:AbsTerm)
(define ML5:AppTerm ML4:AppTerm)
(define ML5:LetRecTerm ML4:LetRecTerm)

(define (ML5:NilTerm) "[]")

(define (ML5:ConsTerm v1 v2) 
  `(,v1 "\\mbox{ :: }" ,v2))

(define (ML5:MatchTerm e1 e2 x y e)
  `("\\mbox{match }" ,e1 "\\mbox{ with } "
    "   [] \\rightarrow " ,e2 
    " \\mid " ,x "\\mbox{ :: } " ,y " \\rightarrow " ,e))

(define ML5:PlusTerm ML4:PlusTerm)
(define ML5:MinusTerm ML4:MinusTerm)
(define ML5:MultTerm ML4:MultTerm)
(define ML5:LtTerm ML4:LtTerm)

(define ML5:EvalTo ML4:EvalTo)

(define ML5:AppBOp ML4:AppBOp)

;; ML6
(define ML6:mv ML5:mv)

(define ML6:FunTerm ML5:FunTerm)
(define ML6:RecTerm ML5:RecTerm)
(define ML6:NilVTerm ML5:NilVTerm)
(define ML6:ConsVTerm ML5:ConsVTerm)

(define ML6:EmptyTerm ML5:EmptyTerm)
(define ML6:BindTerm ML5:BindTerm)

(define (ML6:NilPTerm) "[]")
(define (ML6:ConsPTerm p1 p2)
  `(,p1 "\\mbox{ :: }" ,p2))
(define (ML6:WildPTerm) "\\mbox{_}") ;; not legal TeX but works for LaTeXMathML

(define (ML6:FailTerm) 'fail)

(define (ML6:EmptyCTerm) 'emptyclause)
(define (ML6:AddCTerm p e c)
  (if (eq? c 'emptyclause)
      `(,p "\\rightarrow" ,e)
      `(,p "\\rightarrow" ,e "\\mid" ,c)))

(define ML6:BinOpTerm ML5:BinOpTerm)
(define ML6:IfTerm ML5:IfTerm)
(define ML6:LetTerm ML5:LetTerm)
(define ML6:AbsTerm ML5:AbsTerm)
(define ML6:AppTerm ML5:AppTerm)
(define ML6:LetRecTerm ML5:LetRecTerm)
(define ML6:NilTerm ML5:NilTerm)
(define ML6:ConsTerm ML5:ConsTerm)

(define (ML6:MatchTerm e c)
  `("\\mbox{match }" ,e "\\mbox{ with } " ,c))

(define ML6:PlusTerm ML5:PlusTerm)
(define ML6:MinusTerm ML5:MinusTerm)
(define ML6:MultTerm ML5:MultTerm)
(define ML6:LtTerm ML5:LtTerm)

(define ML6:EvalTo ML5:EvalTo)

(define ML6:AppBOp ML5:AppBOp)

(define (ML6:Matches v p res)
  (cond ((eq? res 'fail)
	 `(,v "\\mbox{ doesn't match }" ,p))
	((equal? res "\\cdot")
	 `(,v "\\mbox{ matches }" ,p " \\mbox{ when }()"))
	(else
	 `(,v "\\mbox{ matches }" ,p " \\mbox{ when }(" ,res ")"))))

;; Typing ML2

(define (TypingML2:TyBoolTerm) "bool")
(define (TypingML2:TyIntTerm) "int")

(define (TypingML2:mv base . suffix)
  (mv base (and (pair? suffix) (car suffix)) '(("env" "\\Gamma")
					       ("t" "\\tau"))))

(define (TypingML2:EmptyTerm) "\\cdot")

(define (TypingML2:BindTerm env x t)
  (if (equal? env "\\cdot")
      `(,x ":" ,t)
      `(,env "," ,x ":" ,t)))

(define TypingML2:BinOpTerm ML2:BinOpTerm)
(define TypingML2:IfTerm ML2:IfTerm)
(define TypingML2:LetTerm ML2:LetTerm)

(define TypingML2:PlusTerm ML2:PlusTerm)
(define TypingML2:MinusTerm ML2:MinusTerm)
(define TypingML2:MultTerm ML2:MultTerm)
(define TypingML2:LtTerm ML2:LtTerm)

(define (TypingML2:Typing env e t)
  `(,env "\\vdash" ,e ":" ,t))

;; TypingML4

(define (TypingML4:TyVarTerm a) a)
(define TypingML4:TyBoolTerm TypingML2:TyBoolTerm)
(define TypingML4:TyIntTerm TypingML2:TyIntTerm)
(define (TypingML4:TyFunTerm t1 t2)
  `(,t1 "\\rightarrow" ,t2))

(define (TypingML4:mv base . suffix)
  (mv base (and (pair? suffix) (car suffix)) '(("env" "\\Gamma")
					       ("t" "\\tau")
					       ("a" "\\alpha"))))

(define TypingML4:EmptyTerm TypingML2:EmptyTerm)

(define TypingML4:BindTerm TypingML2:BindTerm)

(define TypingML4:BinOpTerm TypingML2:BinOpTerm)
(define TypingML4:IfTerm TypingML2:IfTerm)
(define TypingML4:LetTerm TypingML2:LetTerm)

(define TypingML4:AbsTerm ML4:AbsTerm)
(define TypingML4:AppTerm ML4:AppTerm)
(define TypingML4:LetRecTerm ML4:LetRecTerm)

(define TypingML4:PlusTerm TypingML2:PlusTerm)
(define TypingML4:MinusTerm TypingML2:MinusTerm)
(define TypingML4:MultTerm TypingML2:MultTerm)
(define TypingML4:LtTerm TypingML2:LtTerm)

(define TypingML4:Typing TypingML2:Typing)

;; TypingML5

(define TypingML5:TyVarTerm TypingML4:TyVarTerm)
(define TypingML5:TyBoolTerm TypingML4:TyBoolTerm)
(define TypingML5:TyIntTerm TypingML4:TyIntTerm)
(define TypingML5:TyFunTerm TypingML4:TyFunTerm)
(define (TypingML5:TyListTerm t)
  `(,t "\\;list"))

(define TypingML5:mv TypingML4:mv)

(define TypingML5:EmptyTerm TypingML4:EmptyTerm)

(define TypingML5:BindTerm TypingML4:BindTerm)

(define TypingML5:BinOpTerm TypingML4:BinOpTerm)
(define TypingML5:IfTerm TypingML4:IfTerm)
(define TypingML5:LetTerm TypingML4:LetTerm)

(define TypingML5:AbsTerm TypingML4:AbsTerm)
(define TypingML5:AppTerm TypingML4:AppTerm)
(define TypingML5:LetRecTerm TypingML4:LetRecTerm)
(define TypingML5:NilTerm ML5:NilTerm)
(define TypingML5:ConsTerm ML5:ConsTerm)
(define TypingML5:MatchTerm ML5:MatchTerm)

(define TypingML5:PlusTerm TypingML4:PlusTerm)
(define TypingML5:MinusTerm TypingML4:MinusTerm)
(define TypingML5:MultTerm TypingML4:MultTerm)
(define TypingML5:LtTerm TypingML4:LtTerm)

(define TypingML5:Typing TypingML4:Typing)

;; PolyML4

(define PolyML4:TyBoolTerm TypingML4:TyBoolTerm)
(define PolyML4:TyIntTerm TypingML4:TyIntTerm)
(define PolyML4:TyFunTerm TypingML4:TyFunTerm)

(define (PolyML4:TyFVarTerm a) `("'" ,a))
(define (PolyML4:TyBVarTerm i) "")

; (define (PolyML4:TyFVar a))
; (define (PolyML4:TyBVar a))
; (define (PolyML4:TySchemeTerm i t))

(define (PolyML4:mv base . suffix)
  (mv base (and (pair? suffix) (car suffix)) '(("env" "\\Gamma")
					       ("t" "\\tau")
					       ("s" "\\sigma"))))

(define (PolyML4:TySchemeTerm i t) 
  `(,(PolyML4:TyFVarTerm (PolyML4:mv "a" "1"))
    "\\cdots"
    ,(PolyML4:TyFVarTerm (PolyML4:mv "a" "n"))
    "." ,t))
    
(define PolyML4:EmptyTerm TypingML4:EmptyTerm)
(define PolyML4:BindTerm TypingML4:BindTerm)

(define PolyML4:BinOpTerm TypingML4:BinOpTerm)
(define PolyML4:IfTerm TypingML4:IfTerm)
(define PolyML4:LetTerm TypingML4:LetTerm)
(define PolyML4:AbsTerm TypingML4:AbsTerm)
(define PolyML4:AppTerm TypingML4:AppTerm)
(define PolyML4:LetRecTerm TypingML4:LetRecTerm)

(define PolyML4:PlusTerm TypingML4:PlusTerm)
(define PolyML4:MinusTerm TypingML4:MinusTerm)
(define PolyML4:MultTerm TypingML4:MultTerm)
(define PolyML4:LtTerm TypingML4:LtTerm)

(define PolyML4:Typing TypingML4:Typing)

;; PolyML5

(define PolyML5:TyBoolTerm TypingML5:TyBoolTerm)
(define PolyML5:TyIntTerm TypingML5:TyIntTerm)
(define PolyML5:TyFunTerm TypingML5:TyFunTerm)
(define PolyML5:TyListTerm TypingML5:TyListTerm)

(define PolyML5:TyFVarTerm PolyML4:TyFVarTerm)
(define PolyML5:TyBVarTerm PolyML4:TyBVarTerm)
(define PolyML5:TySchemeTerm PolyML4:TySchemeTerm)

(define PolyML5:mv PolyML4:mv)

(define PolyML5:EmptyTerm TypingML5:EmptyTerm)

(define PolyML5:BindTerm TypingML5:BindTerm)

(define PolyML5:BinOpTerm TypingML5:BinOpTerm)
(define PolyML5:IfTerm TypingML5:IfTerm)
(define PolyML5:LetTerm TypingML5:LetTerm)

(define PolyML5:AbsTerm TypingML5:AbsTerm)
(define PolyML5:AppTerm TypingML5:AppTerm)
(define PolyML5:LetRecTerm TypingML5:LetRecTerm)
(define PolyML5:NilTerm TypingML5:NilTerm)
(define PolyML5:ConsTerm TypingML5:ConsTerm)
(define PolyML5:MatchTerm TypingML5:MatchTerm)

(define PolyML5:PlusTerm TypingML5:PlusTerm)
(define PolyML5:MinusTerm TypingML5:MinusTerm)
(define PolyML5:MultTerm TypingML5:MultTerm)
(define PolyML5:LtTerm TypingML5:LtTerm)

(define PolyML5:Typing TypingML5:Typing)
