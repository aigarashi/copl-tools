#! /usr/bin/gosh
;; translation from inference rules to MathML
;;
;; $Id: $

(use text.html-lite)
(use srfi-13)
(use util.list)

(define (normalize-rname rn)
  (string-downcase (string-filter rn char-alphabetic?)))

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
		     :src "http://math.etsu.edu/LaTeXMathML/LaTeXMathML.js")
	(html:link :rel "stylesheet" :type "text/css" 
		   :href "http://math.etsu.edu/LaTeXMathML/LaTeXMathML.standardarticle.css")))

(define (rule-style) ; style of rules
  (style :type "text/css" "
div.rule  { text-align: center; }
span.rname { font-variant: small-caps; }
"))

(define (js:ShowStuff)
  ;; Javascipt function to swap display/no display for all content
  ;; within span tags Click_Menu  (html:script :language "JavaScript" 
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
 -->")

(define (rnameref rn) ; link to call Show_Stuff
  (html:a :href #`"javascript:Show_Stuff(,(normalize-rname rn))" rn))


;;; game specific functions follow

;; Game nat
(define (nat:STerm n)
  `("S(" ,n ")"))

(define (nat:ZTerm) "Z")

(define (nat:PTerm e1 e2)
  `(,e1 "+" ,e2))

(define (nat:MTerm e1 e2)
  `(,e1 "*" ,e2))

(define (nat:EvalTo e n)
  `(,e "\\Downarrow" ,n "\n"))

(define (nat:PlusIs n1 n2 n3)
  `(,n1 "\\mbox{ plus }" ,n2 "\\mbox{ is }" ,n3 "\n"))

(define (nat:MultIs n1 n2 n3)
  `(,n1 "\\mbox{ mult }" ,n2 "\\mbox{ is }" ,n3 "\n"))

;; ML1
(define (ML1:mv base . suffix)
  (mv base (and (pair? suffix) (car suffix)) '()))

(define (ML1:BinOpTerm p e1 e2)
  `(,e1 ,p ,e2))

(define (ML1:IfTerm e1 e2 e3)
  `("\\mbox{if }" ,e1 "\\mbox{ then }" ,e2 "\\mbox{ else }" ,e3))

(define (ML1:PlusTerm) '+)
(define (ML1:MinusTerm) '-)
(define (ML1:MultTerm) '*)
(define (ML1:LtTerm) '<)

(define (ML1:EvalTo e v)
  `(,e "\\Downarrow" ,v "\n"))

(define (ML1:AppBOp p v1 v2 v3)
  (let ((p' (cadr (assq p '((+ "\\mbox{ plus }")
			    (- "\\mbox{ minus }")
			    (* "\\mbox{ mult }")
			    (< "\\mbox{ less than }"))))))
    `(,v1 ,p' ,v2 "\\mbox{ is }" ,v3)))

;; ML2
(define (ML2:mv base . suffix)
  (mv base (and (pair? suffix) (car suffix)) '(("env" "\\Gamma"))))

(define (ML2:Empty ""))  ;; no need to define

(define (ML2:Bind env x v)
  `(,env "," ,x ":" ,v))

(define ML2:BinOpTerm ML1:BinOpTerm)
(define ML2:IfTerm ML1:If)

(define (ML2:LetTerm x e1 e2)
  `("\mbox{let }" ,x "=" ,e1 "\mbox{ in }" ,e2))

(define ML2:PlusTerm ML1:PlusTerm)
(define ML2:MinusTerm ML1:MinusTerm)
(define ML2:MultTerm ML1:MultTerm)
(define ML2:LtTerm ML1:LtTerm)

(define (ML2:EvalTo env e v)
  `(,env "\\vdash" ,@(ML1:EvalTo e v)))

(define ML2:AppBOp ML1:AppBOp)

