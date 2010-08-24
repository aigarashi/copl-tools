#! /usr/bin/gosh
;-*-Scheme-*-
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
      (html:h1 (cadr args))
      (html:h2 "Syntax:")
      (format-bnf bnfdefs)
      (html:h2 "Derivation Rules:")
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

;; EvalML1
(define (EvalML1:mv base . suffix)
  (mv base (and (pair? suffix) (car suffix)) '()))

(define (EvalML1:BinOpTerm p e1 e2)
  `(,e1 "\\," ,p "\\," ,e2))

(define (EvalML1:IfTerm e1 e2 e3)
  `("\\mbox{if }" ,e1 "\\mbox{ then }" ,e2 "\\mbox{ else }" ,e3))

(define (EvalML1:PlusTerm) '+)
(define (EvalML1:MinusTerm) '-)
(define (EvalML1:MultTerm) '*)
(define (EvalML1:LtTerm) '<)

(define (EvalML1:EvalTo e v)
  `(,e "\\Downarrow" ,v))

(define (EvalML1:AppBOp p v1 v2 v3)
  (let ((p (cadr (assq p '((+ "\\mbox{ plus }")
			   (- "\\mbox{ minus }")
			   (* "\\mbox{ times }")
			   (< "\\mbox{ less than }"))))))
    `(,v1 ,p ,v2 "\\mbox{ is }" ,v3)))

;; EvalML1Err
(define EvalML1Err:mv EvalML1:mv)

(define EvalML1Err:BinOpTerm EvalML1:BinOpTerm)

(define EvalML1Err:IfTerm EvalML1:IfTerm)

(define EvalML1Err:PlusTerm EvalML1:PlusTerm)
(define EvalML1Err:MinusTerm EvalML1:MinusTerm)
(define EvalML1Err:MultTerm EvalML1:MultTerm)
(define EvalML1Err:LtTerm EvalML1:LtTerm)

(define (EvalML1Err:ErrorTerm) "\\mbox{error}")

(define EvalML1Err:EvalTo EvalML1:EvalTo)
(define EvalML1Err:AppBOp EvalML1:AppBOp)


;; EvalML2
(define (EvalML2:mv base . suffix)
  (mv base (and (pair? suffix) (car suffix)) '(("env" "\\mathcal{E}"))))

(define (EvalML2:EmptyTerm) "\\cdot")

(define (EvalML2:BindTerm env x v)
  (if (equal? env "\\cdot")
      `(,x "=" ,v)
      `(,env "," ,x "=" ,v)))

(define EvalML2:BinOpTerm EvalML1:BinOpTerm)
(define EvalML2:IfTerm EvalML1:IfTerm)

(define (EvalML2:LetTerm x e1 e2)
  `("\\mbox{let }" ,x " = " ,e1 "\\mbox{ in }" ,e2))

(define EvalML2:PlusTerm EvalML1:PlusTerm)
(define EvalML2:MinusTerm EvalML1:MinusTerm)
(define EvalML2:MultTerm EvalML1:MultTerm)
(define EvalML2:LtTerm EvalML1:LtTerm)

(define (EvalML2:EvalTo env e v)
  `(,env "\\vdash" ,@(EvalML1:EvalTo e v)))

(define EvalML2:AppBOp EvalML1:AppBOp)

;; EvalML3
(define EvalML3:mv EvalML2:mv)

(define (EvalML3:FunTerm env x e)
  `("\\mbox{(}" ,env "\\mbox{)[fun }" ,x "\\rightarrow" ,e "\\mbox{]}"))

(define (EvalML3:RecTerm env x y e)
  `("\\mbox{(}" ,env 
    "\\mbox{)[rec }" ,x " = \\mbox{fun }" ,y "\\rightarrow" ,e "\\mbox{]}"))

(define EvalML3:EmptyTerm EvalML2:EmptyTerm)
(define EvalML3:BindTerm EvalML2:BindTerm)
(define EvalML3:BinOpTerm EvalML2:BinOpTerm)
(define EvalML3:IfTerm EvalML2:IfTerm)
(define EvalML3:LetTerm EvalML2:LetTerm)

(define (EvalML3:AbsTerm x e)
  `("\\mbox{fun }" ,x "\\rightarrow" ,e))

(define (EvalML3:AppTerm e1 e2)
  `(,e1 "\\;" ,e2))

(define (EvalML3:LetRecTerm x y e1 e2)
    `("\\mbox{let rec }" ,x " = \\mbox{fun }" ,y " \\rightarrow " ,e1 "\\mbox{ in }" ,e2))

(define EvalML3:PlusTerm EvalML2:PlusTerm)
(define EvalML3:MinusTerm EvalML2:MinusTerm)
(define EvalML3:MultTerm EvalML2:MultTerm)
(define EvalML3:LtTerm EvalML2:LtTerm)

(define EvalML3:EvalTo EvalML2:EvalTo)

(define EvalML3:AppBOp EvalML2:AppBOp)

;; EvalRefML3
(define (EvalRefML3:mv base . suffix)
  (mv base (and (pair? suffix) (car suffix)) 
      '(("env" "\\mathcal{E}") ("s" "S"))))

(define EvalRefML3:FunTerm EvalML3:FunTerm)
(define EvalRefML3:RecTerm EvalML3:RecTerm)

(define EvalRefML3:EmptyTerm EvalML3:EmptyTerm)
(define EvalRefML3:BindTerm EvalML3:BindTerm)

(define (EvalRefML3:EmptySTerm) "\\cdot")

(define (EvalRefML3:BlockTerm store x v)
  (if (equal? store "\\cdot")
      `(,x "=" ,v)
      `(,store ", " ,x "=" ,v)))

(define EvalRefML3:BinOpTerm EvalML3:BinOpTerm)
(define EvalRefML3:IfTerm EvalML3:IfTerm)
(define EvalRefML3:LetTerm EvalML3:LetTerm)
(define EvalRefML3:AbsTerm EvalML3:AbsTerm)
(define EvalRefML3:AppTerm EvalML3:AppTerm)
(define EvalRefML3:LetRecTerm EvalML3:LetRecTerm)
(define (EvalRefML3:NewRefTerm e)
  `("\\mbox{ref }" ,e))
(define (EvalRefML3:DerefTerm e)
  `("!" ,e))
(define (EvalRefML3:AssignTerm e1 e2)
  `(,e1 " := " ,e2))

(define EvalRefML3:PlusTerm EvalML3:PlusTerm)
(define EvalRefML3:MinusTerm EvalML3:MinusTerm)
(define EvalRefML3:MultTerm EvalML3:MultTerm)
(define EvalRefML3:LtTerm EvalML3:LtTerm)

(define (EvalRefML3:EvalTo s1 env e v s2)
  `(,s1 " / " ,@(EvalML3:EvalTo env e v) " / " ,s2))

(define EvalRefML3:AppBOp EvalML3:AppBOp)

;; EvalML4
(define EvalML4:mv EvalML3:mv)

(define EvalML4:FunTerm EvalML3:FunTerm)
(define EvalML4:RecTerm EvalML3:RecTerm)
(define (EvalML4:NilVTerm) "[]")
(define (EvalML4:ConsVTerm v1 v2) 
  `(,v1 "\\mbox{ :: }" ,v2))

(define EvalML4:EmptyTerm EvalML3:EmptyTerm)
(define EvalML4:BindTerm EvalML3:BindTerm)
(define EvalML4:BinOpTerm EvalML3:BinOpTerm)
(define EvalML4:IfTerm EvalML3:IfTerm)
(define EvalML4:LetTerm EvalML3:LetTerm)
(define EvalML4:AbsTerm EvalML3:AbsTerm)
(define EvalML4:AppTerm EvalML3:AppTerm)
(define EvalML4:LetRecTerm EvalML3:LetRecTerm)

(define (EvalML4:NilTerm) "[]")

(define (EvalML4:ConsTerm v1 v2) 
  `(,v1 "\\mbox{ :: }" ,v2))

(define (EvalML4:MatchTerm e1 e2 x y e)
  `("\\mbox{match }" ,e1 "\\mbox{ with } "
    "   [] \\rightarrow " ,e2 
    " \\mid " ,x "\\mbox{ :: } " ,y " \\rightarrow " ,e))

(define EvalML4:PlusTerm EvalML3:PlusTerm)
(define EvalML4:MinusTerm EvalML3:MinusTerm)
(define EvalML4:MultTerm EvalML3:MultTerm)
(define EvalML4:LtTerm EvalML3:LtTerm)

(define EvalML4:EvalTo EvalML3:EvalTo)

(define EvalML4:AppBOp EvalML3:AppBOp)

;; EvalML5
(define EvalML5:mv EvalML4:mv)

(define EvalML5:FunTerm EvalML4:FunTerm)
(define EvalML5:RecTerm EvalML4:RecTerm)
(define EvalML5:NilVTerm EvalML4:NilVTerm)
(define EvalML5:ConsVTerm EvalML4:ConsVTerm)

(define EvalML5:EmptyTerm EvalML4:EmptyTerm)
(define EvalML5:BindTerm EvalML4:BindTerm)

(define (EvalML5:NilPTerm) "[]")
(define (EvalML5:ConsPTerm p1 p2)
  `(,p1 "\\mbox{ :: }" ,p2))
(define (EvalML5:WildPTerm) "\\mbox{_}") ;; not legal TeX but works for LaTeXMathML

(define (EvalML5:FailTerm) 'fail)

(define (EvalML5:EmptyCTerm) 'emptyclause)
(define (EvalML5:AddCTerm p e c)
  (if (eq? c 'emptyclause)
      `(,p "\\rightarrow" ,e)
      `(,p "\\rightarrow" ,e "\\mid" ,c)))

(define EvalML5:BinOpTerm EvalML4:BinOpTerm)
(define EvalML5:IfTerm EvalML4:IfTerm)
(define EvalML5:LetTerm EvalML4:LetTerm)
(define EvalML5:AbsTerm EvalML4:AbsTerm)
(define EvalML5:AppTerm EvalML4:AppTerm)
(define EvalML5:LetRecTerm EvalML4:LetRecTerm)
(define EvalML5:NilTerm EvalML4:NilTerm)
(define EvalML5:ConsTerm EvalML4:ConsTerm)

(define (EvalML5:MatchTerm e c)
  `("\\mbox{match }" ,e "\\mbox{ with } " ,c))

(define EvalML5:PlusTerm EvalML4:PlusTerm)
(define EvalML5:MinusTerm EvalML4:MinusTerm)
(define EvalML5:MultTerm EvalML4:MultTerm)
(define EvalML5:LtTerm EvalML4:LtTerm)

(define EvalML5:EvalTo EvalML4:EvalTo)

(define EvalML5:AppBOp EvalML4:AppBOp)

(define (EvalML5:Matches v p res)
  (cond ((eq? res 'fail)
	 `(,v "\\mbox{ doesn't match }" ,p))
	((equal? res "\\cdot")
	 `(,v "\\mbox{ matches }" ,p " \\mbox{ when }()"))
	(else
	 `(,v "\\mbox{ matches }" ,p " \\mbox{ when }(" ,res ")"))))

;; EvalML6
(define EvalML6:mv EvalML5:mv)

(define EvalML6:FunTerm EvalML5:FunTerm)
(define EvalML6:RecTerm EvalML5:RecTerm)
(define (EvalML6:CnstrVTerm c) c)
(define (EvalML6:CnstrViTerm c v)
  `(,c "\\;" ,v))
(define (EvalML6:CnstrViiTerm c v1 v2)
  `(,c "\\mbox{(}" ,v1 "\\mbox{,}" ,v2 "\\mbox{)}"))

(define EvalML6:EmptyTerm EvalML5:EmptyTerm)
(define EvalML6:BindTerm EvalML5:BindTerm)

(define (EvalML6:WildPTerm) "\\mbox{_}") ;; not legal TeX but works for LaTeXMathML
(define (EvalML6:CnstrPTerm c) c)
(define (EvalML6:CnstrPiTerm c p)
  `(,c "\\;" ,p ))
(define (EvalML6:CnstrPiiTerm c p1 p2)
  `(,c "\\mbox{(}" ,p1 "\\mbox{,}" ,p2 "\\mbox{)}"))

(define (EvalML6:FailTerm) 'fail)

(define (EvalML6:EmptyCTerm) 'emptyclause)
(define (EvalML6:AddCTerm p e c)
  (if (eq? c 'emptyclause)
      `(,p "\\rightarrow" ,e)
      `(,p "\\rightarrow" ,e "\\mid" ,c)))

(define EvalML6:BinOpTerm EvalML5:BinOpTerm)
(define EvalML6:IfTerm EvalML5:IfTerm)
(define EvalML6:LetTerm EvalML5:LetTerm)
(define EvalML6:AbsTerm EvalML5:AbsTerm)
(define EvalML6:AppTerm EvalML5:AppTerm)
(define EvalML6:LetRecTerm EvalML5:LetRecTerm)
(define (EvalML6:CnstrETerm c) c)
(define (EvalML6:CnstrEiTerm c e)
  `(,c "\\;" ,e ))
(define (EvalML6:CnstrEiiTerm c e1 e2)
  `(,c "\\mbox{(}" ,e1 "\\mbox{,}" ,e2 "\\mbox{)}"))

(define (EvalML6:MatchTerm e c)
  `("\\mbox{match }" ,e "\\mbox{ with } " ,c))

(define EvalML6:PlusTerm EvalML5:PlusTerm)
(define EvalML6:MinusTerm EvalML5:MinusTerm)
(define EvalML6:MultTerm EvalML5:MultTerm)
(define EvalML6:LtTerm EvalML5:LtTerm)

(define EvalML6:EvalTo EvalML5:EvalTo)

(define EvalML6:AppBOp EvalML5:AppBOp)

(define (EvalML6:Matches v p res)
  (cond ((eq? res 'fail)
	 `(,v "\\mbox{ doesn't match }" ,p))
	((equal? res "\\cdot")
	 `(,v "\\mbox{ matches }" ,p " \\mbox{ when }()"))
	(else
	 `(,v "\\mbox{ matches }" ,p " \\mbox{ when }(" ,res ")"))))

;; EvalNamelessML3
(define (EvalML2:mv base . suffix)
  (mv base (and (pair? suffix) (car suffix)) '(("env" "\\mathcal{E}")
					       ("v" "w"))))

(define (EvalNamelessML3:FunTerm env e)
  `("\\mbox{(}" ,env "\\mbox{)[fun }. \\rightarrow" ,e "\\mbox{]}"))

(define (EvalNamelessML3:RecTerm env e)
  `("\\mbox{(}" ,env 
    "\\mbox{)[rec .} = \\mbox{fun }. \\rightarrow" ,e "\\mbox{]}"))

(define EvalNamelessML3:EmptyTerm EvalML3:EmptyTerm)
(define (EvalNamelessML3:BindTerm env v)
  (if (equal? env "\\cdot")
      v
      `(,env "," ,v)))

(define (EvalNamelessML3:IndexTerm n) `("#" ,n))
(define EvalNamelessML3:BinOpDTerm EvalML3:BinOpTerm)
(define EvalNamelessML3:IfDTerm EvalML3:IfTerm)
(define (EvalNamelessML3:LetDTerm e1 e2)
  `("\\mbox{let } . = " ,e1 "\\mbox{ in }" ,e2))

(define (EvalNamelessML3:AbsDTerm e)
  `("\\mbox{fun } . \\rightarrow" ,e))
(define EvalNamelessML3:AppDTerm EvalML3:AppTerm)
(define (EvalNamelessML3:LetRecDTerm e1 e2)
    `("\\mbox{let rec } . = \\mbox{fun } . \\rightarrow " ,e1 "\\mbox{ in }" ,e2))

(define EvalNamelessML3:PlusTerm EvalML3:PlusTerm)
(define EvalNamelessML3:MinusTerm EvalML3:MinusTerm)
(define EvalNamelessML3:MultTerm EvalML3:MultTerm)
(define EvalNamelessML3:LtTerm EvalML3:LtTerm)

(define EvalNamelessML3:EvalTo EvalML3:EvalTo)
(define EvalNamelessML3:AppBOp EvalML3:AppBOp)

;; NamelessML3
(define NamelessML3:mv EvalML3:mv)

(define NamelessML3:EmptyTerm EvalML3:EmptyTerm)
(define (NamelessML3:BindTerm env x)
  (if (equal? env "\\cdot")
      v
      `(,env "," ,x)))

(define NamelessML3:BinOpTerm EvalML3:BinOpTerm)
(define NamelessML3:IfTerm EvalML3:IfTerm)
(define NamelessML3:LetTerm EvalML3:LetTerm)
(define NamelessML3:AbsTerm EvalML3:AbsTerm)
(define NamelessML3:AppTerm EvalML3:AppTerm)
(define NamelessML3:LetRecTerm EvalML3:LetRecTerm)

(define NamelessML3:PlusTerm EvalML3:PlusTerm)
(define NamelessML3:MinusTerm EvalML3:MinusTerm)
(define NamelessML3:MultTerm EvalML3:MultTerm)
(define NamelessML3:LtTerm EvalML3:LtTerm)

(define NamelessML3:IndexTerm EvalNamelessML3:IndexTerm)
(define NamelessML3:BinOpDTerm EvalNamelessML3:BinOpDTerm)
(define NamelessML3:IfDTerm EvalNamelessML3:IfDTerm)
(define NamelessML3:LetDTerm EvalNamelessML3:LetDTerm)
(define NamelessML3:AbsDTerm EvalNamelessML3:AbsDTerm)
(define NamelessML3:AppDTerm EvalNamelessML3:AppDTerm)
(define NamelessML3:LetRecDTerm EvalNamelessML3:LetRecDTerm)

(define (NamelessML3:TranslateTo env e d)
  `(,env "\\vdash" ,e "\\Rightarrow" ,d))

(define NamelessML3:AppBOp EvalML3:AppBOp)

;; TypingML2

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

(define TypingML2:BinOpTerm EvalML2:BinOpTerm)
(define TypingML2:IfTerm EvalML2:IfTerm)
(define TypingML2:LetTerm EvalML2:LetTerm)

(define TypingML2:PlusTerm EvalML2:PlusTerm)
(define TypingML2:MinusTerm EvalML2:MinusTerm)
(define TypingML2:MultTerm EvalML2:MultTerm)
(define TypingML2:LtTerm EvalML2:LtTerm)

(define (TypingML2:Typing env e t)
  `(,env "\\vdash" ,e ":" ,t))

;; TypingML3

(define (TypingML3:TyVarTerm a) a)
(define TypingML3:TyBoolTerm TypingML2:TyBoolTerm)
(define TypingML3:TyIntTerm TypingML2:TyIntTerm)
(define (TypingML3:TyFunTerm t1 t2)
  `(,t1 "\\rightarrow" ,t2))

(define (TypingML3:mv base . suffix)
  (mv base (and (pair? suffix) (car suffix)) '(("env" "\\Gamma")
					       ("t" "\\tau")
					       ("a" "\\alpha"))))

(define TypingML3:EmptyTerm TypingML2:EmptyTerm)

(define TypingML3:BindTerm TypingML2:BindTerm)

(define TypingML3:BinOpTerm TypingML2:BinOpTerm)
(define TypingML3:IfTerm TypingML2:IfTerm)
(define TypingML3:LetTerm TypingML2:LetTerm)

(define TypingML3:AbsTerm EvalML3:AbsTerm)
(define TypingML3:AppTerm EvalML3:AppTerm)
(define TypingML3:LetRecTerm EvalML3:LetRecTerm)

(define TypingML3:PlusTerm TypingML2:PlusTerm)
(define TypingML3:MinusTerm TypingML2:MinusTerm)
(define TypingML3:MultTerm TypingML2:MultTerm)
(define TypingML3:LtTerm TypingML2:LtTerm)

(define TypingML3:Typing TypingML2:Typing)

;; TypingML4

(define TypingML4:TyVarTerm TypingML3:TyVarTerm)
(define TypingML4:TyBoolTerm TypingML3:TyBoolTerm)
(define TypingML4:TyIntTerm TypingML3:TyIntTerm)
(define TypingML4:TyFunTerm TypingML3:TyFunTerm)
(define (TypingML4:TyListTerm t)
  `(,t "\\;list"))

(define TypingML4:mv TypingML3:mv)

(define TypingML4:EmptyTerm TypingML3:EmptyTerm)

(define TypingML4:BindTerm TypingML3:BindTerm)

(define TypingML4:BinOpTerm TypingML3:BinOpTerm)
(define TypingML4:IfTerm TypingML3:IfTerm)
(define TypingML4:LetTerm TypingML3:LetTerm)

(define TypingML4:AbsTerm TypingML3:AbsTerm)
(define TypingML4:AppTerm TypingML3:AppTerm)
(define TypingML4:LetRecTerm TypingML3:LetRecTerm)
(define TypingML4:NilTerm EvalML4:NilTerm)
(define TypingML4:ConsTerm EvalML4:ConsTerm)
(define TypingML4:MatchTerm EvalML4:MatchTerm)

(define TypingML4:PlusTerm TypingML3:PlusTerm)
(define TypingML4:MinusTerm TypingML3:MinusTerm)
(define TypingML4:MultTerm TypingML3:MultTerm)
(define TypingML4:LtTerm TypingML3:LtTerm)

(define TypingML4:Typing TypingML3:Typing)

;; TypingML6
(define (TypingML6:mv base . suffix)
  (mv base (and (pair? suffix) (car suffix)) '(("env" "\\Gamma")
					       ("t" "\\tau")
					       ("sg" "\\Sigma")
					       ("tn" "t"))))

(define TypingML6:TyVarTerm TypingML4:TyVarTerm)
(define TypingML6:TyBoolTerm TypingML4:TyBoolTerm)
(define TypingML6:TyIntTerm TypingML4:TyIntTerm)
(define TypingML6:TyFunTerm TypingML4:TyFunTerm)

(define (TypingML6:CnstrTTerm tn) tn)
(define (TypingML6:CnstrTiTerm t tn)
  `(,t "\\rightarrow" ,tn))
(define (TypingML6:CnstrTiiTerm t1 t2 tn)
  `(,t1 "\\times" ,t2 "\\rightarrow" ,tn))

(define TypingML6:EmptyTerm TypingML4:EmptyTerm)
(define TypingML6:BindTerm TypingML4:BindTerm)

(define TypingML6:EmptySTerm TypingML4:EmptyTerm)
(define (TypingML6:BindSTerm sig c ct)
  (if (equal? sig "\\cdot")
      `(,c ":" ,ct)
      `(,sig "," ,c ":" ,ct)))


(define TypingML6:WildPTerm EvalML6:WildPTerm)
(define TypingML6:CnstrPTerm EvalML6:CnstrPTerm)
(define TypingML6:CnstrPiTerm EvalML6:CnstrPiTerm)
(define TypingML6:CnstrPiiTerm EvalML6:CnstrPiiTerm)

(define TypingML6:EmptyCTerm EvalML6:EmptyCTerm)
(define TypingML6:AddCTerm EvalML6:AddCTerm)

(define TypingML6:BinOpTerm EvalML6:BinOpTerm)
(define TypingML6:IfTerm EvalML6:IfTerm)
(define TypingML6:LetTerm EvalML6:LetTerm)
(define TypingML6:AbsTerm EvalML6:AbsTerm)
(define TypingML6:AppTerm EvalML6:AppTerm)
(define TypingML6:LetRecTerm EvalML6:LetRecTerm)
(define TypingML6:CnstrETerm EvalML6:CnstrETerm)
(define TypingML6:CnstrEiTerm EvalML6:CnstrEiTerm)
(define TypingML6:CnstrEiiTerm EvalML6:CnstrEiiTerm)

(define TypingML6:MatchTerm EvalML6:MatchTerm)

(define TypingML6:PlusTerm EvalML6:PlusTerm)
(define TypingML6:MinusTerm EvalML6:MinusTerm)
(define TypingML6:MultTerm EvalML6:MultTerm)
(define TypingML6:LtTerm EvalML6:LtTerm)

(define TypingML6:EvalTo EvalML6:EvalTo)

(define TypingML6:AppBOp EvalML6:AppBOp)

(define (TypingML6:PatTyping sg t p env)
  (cond ((equal? env "\\cdot")
	 `(,t "\\mbox{ matches}_{" ,sg "}\\;" ,p " \\mbox{ when }()"))
	(else
	 `(,t "\\mbox{ matches}_{" ,sg "}\\;" ,p " \\mbox{ when }(" ,env ")"))))

(define (TypingML6:Typing sg env e t)
  `(,env "\\vdash_{" ,sg "}" ,e ":" ,t))

;; PolyTypingML3

(define PolyTypingML3:TyBoolTerm TypingML3:TyBoolTerm)
(define PolyTypingML3:TyIntTerm TypingML3:TyIntTerm)
(define PolyTypingML3:TyFunTerm TypingML3:TyFunTerm)

(define (PolyTypingML3:TyFVarTerm a) `("'" ,a))
(define (PolyTypingML3:TyBVarTerm i) "")

; (define (PolyTypingML3:TyFVar a))
; (define (PolyTypingML3:TyBVar a))
; (define (PolyTypingML3:TySchemeTerm i t))

(define (PolyTypingML3:mv base . suffix)
  (mv base (and (pair? suffix) (car suffix)) '(("env" "\\Gamma")
					       ("t" "\\tau")
					       ("s" "\\sigma"))))

(define (PolyTypingML3:TySchemeTerm i t) 
  `(,(PolyTypingML3:TyFVarTerm (PolyTypingML3:mv "a" "1"))
    "\\cdots"
    ,(PolyTypingML3:TyFVarTerm (PolyTypingML3:mv "a" "n"))
    "." ,t))
    
(define PolyTypingML3:EmptyTerm TypingML3:EmptyTerm)
(define PolyTypingML3:BindTerm TypingML3:BindTerm)

(define PolyTypingML3:BinOpTerm TypingML3:BinOpTerm)
(define PolyTypingML3:IfTerm TypingML3:IfTerm)
(define PolyTypingML3:LetTerm TypingML3:LetTerm)
(define PolyTypingML3:AbsTerm TypingML3:AbsTerm)
(define PolyTypingML3:AppTerm TypingML3:AppTerm)
(define PolyTypingML3:LetRecTerm TypingML3:LetRecTerm)

(define PolyTypingML3:PlusTerm TypingML3:PlusTerm)
(define PolyTypingML3:MinusTerm TypingML3:MinusTerm)
(define PolyTypingML3:MultTerm TypingML3:MultTerm)
(define PolyTypingML3:LtTerm TypingML3:LtTerm)

(define PolyTypingML3:Typing TypingML3:Typing)

;; PolyTypingML4

(define PolyTypingML4:TyBoolTerm TypingML4:TyBoolTerm)
(define PolyTypingML4:TyIntTerm TypingML4:TyIntTerm)
(define PolyTypingML4:TyFunTerm TypingML4:TyFunTerm)
(define PolyTypingML4:TyListTerm TypingML4:TyListTerm)

(define PolyTypingML4:TyFVarTerm PolyTypingML3:TyFVarTerm)
(define PolyTypingML4:TyBVarTerm PolyTypingML3:TyBVarTerm)
(define PolyTypingML4:TySchemeTerm PolyTypingML3:TySchemeTerm)

(define PolyTypingML4:mv PolyTypingML3:mv)

(define PolyTypingML4:EmptyTerm TypingML4:EmptyTerm)

(define PolyTypingML4:BindTerm TypingML4:BindTerm)

(define PolyTypingML4:BinOpTerm TypingML4:BinOpTerm)
(define PolyTypingML4:IfTerm TypingML4:IfTerm)
(define PolyTypingML4:LetTerm TypingML4:LetTerm)

(define PolyTypingML4:AbsTerm TypingML4:AbsTerm)
(define PolyTypingML4:AppTerm TypingML4:AppTerm)
(define PolyTypingML4:LetRecTerm TypingML4:LetRecTerm)
(define PolyTypingML4:NilTerm TypingML4:NilTerm)
(define PolyTypingML4:ConsTerm TypingML4:ConsTerm)
(define PolyTypingML4:MatchTerm TypingML4:MatchTerm)

(define PolyTypingML4:PlusTerm TypingML4:PlusTerm)
(define PolyTypingML4:MinusTerm TypingML4:MinusTerm)
(define PolyTypingML4:MultTerm TypingML4:MultTerm)
(define PolyTypingML4:LtTerm TypingML4:LtTerm)

(define PolyTypingML4:Typing TypingML4:Typing)

;; EvalContML1

(define EvalContML1:mv EvalML1:mv)
(define EvalContML1:BinOpTerm EvalML1:BinOpTerm)
(define EvalContML1:IfTerm EvalML1:IfTerm)
(define EvalContML1:PlusTerm EvalML1:PlusTerm)
(define EvalContML1:MinusTerm EvalML1:MinusTerm)
(define EvalContML1:MultTerm EvalML1:MultTerm)
(define EvalContML1:LtTerm EvalML1:LtTerm)

(define Hole "\\mbox{_}")
(define (EvalContML1:RetKTerm) Hole)
(define (EvalContML1:EvalRKTerm e op k)
 `("\\{" ,(EvalContML1:BinOpTerm op Hole e) "\\} \\gg" ,k))
(define (EvalContML1:AppOpKTerm v op k)
 `("\\{" ,(EvalContML1:BinOpTerm op v Hole) "\\} \\gg" ,k))
(define (EvalContML1:BranchKTerm e1 e2 k)
 `("\\{" ,(EvalContML1:IfTerm Hole e1 e2) "\\} \\gg" ,k))

(define (EvalContML1:EvalTo k e v)
  `(,e "\\gg" ,k "\\Downarrow" ,v))

(define EvalContML1:AppBOp EvalML1:AppBOp)
(define (EvalContML1:AppK k v1 v2)
  `(,v1 "\\Rightarrow" ,k "\\Downarrow" ,v2))

;; EvalContML3
(define EvalContML3:mv EvalML3:mv)

(define EvalContML3:FunTerm EvalML3:FunTerm)
(define EvalContML3:RecTerm EvalML3:RecTerm)
(define (EvalContML3:ContFTerm k)
  `("[" ,k "]"))

(define EvalContML3:EmptyTerm EvalML3:EmptyTerm)
(define EvalContML3:BindTerm EvalML3:BindTerm)
(define EvalContML3:BinOpTerm EvalML3:BinOpTerm)
(define EvalContML3:IfTerm EvalML3:IfTerm)
(define EvalContML3:LetTerm EvalML3:LetTerm)
(define EvalContML3:AbsTerm EvalML3:AbsTerm)
(define EvalContML3:AppTerm EvalML3:AppTerm)
(define EvalContML3:LetRecTerm EvalML3:LetRecTerm)
(define (EvalContML3:LetCcTerm x e)
  `("\\mbox{letcc }" ,x "\\mbox{ in }" ,e))

(define (EvalContML3:RetKTerm) '("\\mbox{_}"))
(define (EvalContML3:EvalRKTerm env e op k)
  `("\\{" ,env "\\vdash " ,@(EvalML1:BinOpTerm op "\\mbox{_}" e) "\\} \\gg " ,k))
(define (EvalContML3:AppOpKTerm v op k)
  `("\\{" ,@(EvalML1:BinOpTerm op v "\\mbox{_}") "\\} \\gg " ,k))
(define (EvalContML3:BranchKTerm env e1 e2 k)
  `("\\{" ,env "\\vdash \\mbox{if _ then }" ,e1 "\\mbox{ else }" ,e2 "\\} \\gg" ,k))
(define (EvalContML3:LetBodyKTerm env x e k)
  `("\\{" ,env "\\vdash \\mbox{let }" ,x "= \\mbox{_} \\mbox{ in } " ,e "\\} \\gg k"))
(define (EvalContML3:EvalArgKTerm env e k)
  `("\\{" ,env "\\vdash \\mbox{_}\\, " ,e "\\} \\gg" ,k))
(define (EvalContML3:AppFunKTerm v k)
  `("\\{" ,v "\\,\\mbox{_}\\} \\gg " ,k))

(define EvalContML3:PlusTerm EvalML3:PlusTerm)
(define EvalContML3:MinusTerm EvalML3:MinusTerm)
(define EvalContML3:MultTerm EvalML3:MultTerm)
(define EvalContML3:LtTerm EvalML3:LtTerm)

(define (EvalContML3:EvalTo env k e v)
  `(,env "\\vdash" ,e " \\gg " ,k "\\Downarrow" ,v))
(define EvalContML3:AppBOp EvalML3:AppBOp)
(define (EvalContML3:AppK k v1 v2)
  `(,v1 "\\Rightarrow" ,k "\\Downarrow" ,v2))


;; EvalContML4
(define EvalContML4:mv EvalML4:mv)

(define EvalContML4:FunTerm EvalML4:FunTerm)
(define EvalContML4:RecTerm EvalML4:RecTerm)
(define EvalContML4:NilVTerm EvalML4:NilVTerm)
(define EvalContML4:ConsVTerm EvalML4:ConsVTerm)
(define EvalContML4:ContFTerm EvalContML3:ContFTerm)

(define EvalContML4:EmptyTerm EvalContML3:EmptyTerm)
(define EvalContML4:BindTerm EvalContML3:BindTerm)

(define EvalContML4:BinOpTerm EvalML4:BinOpTerm)
(define EvalContML4:IfTerm EvalML4:IfTerm)
(define EvalContML4:LetTerm EvalML4:LetTerm)
(define EvalContML4:AbsTerm EvalML4:AbsTerm)
(define EvalContML4:AppTerm EvalML4:AppTerm)
(define EvalContML4:LetRecTerm EvalML4:LetRecTerm)
(define EvalContML4:NilTerm EvalML4:NilTerm)
(define EvalContML4:ConsTerm EvalML4:ConsTerm)
(define EvalContML4:MatchTerm EvalML4:MatchTerm)

(define EvalContML4:LetCcTerm EvalContML3:LetCcTerm)

(define EvalContML4:PlusTerm EvalML4:PlusTerm)
(define EvalContML4:MinusTerm EvalML4:MinusTerm)
(define EvalContML4:MultTerm EvalML4:MultTerm)
(define EvalContML4:LtTerm EvalML4:LtTerm)

(define EvalContML4:RetKTerm EvalContML3:RetKTerm)
(define EvalContML4:EvalRKTerm EvalContML3:EvalRKTerm)
(define EvalContML4:AppOpKTerm EvalContML3:AppOpKTerm)
(define EvalContML4:BranchKTerm EvalContML3:BranchKTerm)
(define EvalContML4:LetBodyKTerm EvalContML3:LetBodyKTerm)
(define EvalContML4:EvalArgKTerm EvalContML3:EvalArgKTerm)
(define EvalContML4:AppFunKTerm EvalContML3:AppFunKTerm)
(define (EvalContML4:EvalConsRKTerm env e k)
  `("\\{" ,env "\\vdash" ,(EvalContML4:ConsTerm Hole e) "\\} \\gg" ,k)
)
(define (EvalContML4:ConsKTerm v k)
  `("\\{" ,(EvalContML4:ConsVTerm v Hole) "\\} \\gg" ,k)
)
(define (EvalContML4:MatchKTerm env e1 x y e2 k)
  `("\\{" ,env "\\vdash" ,(EvalContML4:MatchTerm Hole e1 x y e2) "\\} \\gg" ,k)
)

(define EvalContML4:EvalTo EvalContML3:EvalTo)
(define EvalContML4:AppBOp EvalContML3:AppBOp)
(define EvalContML4:AppK EvalContML3:AppK)

