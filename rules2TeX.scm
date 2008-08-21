#! /usr/bin/gosh
;; translation from inference rules to MathML
;;
;; $id: $

(use text.html-lite)
(use srfi-13)
(use util.list)

(define (normalize-rname rn)
  (string-downcase (string-filter rn char-alphabetic?)))

(define (infrule name premises concl)
  (let ((delimited-premises (intersperse " \\qquad " premises))
	(nameID (normalize-rname name)))
    (html:div 
     :id nameID :class "rule" :style "display: none;"
     (html:pre 
      :class "TeX"
      (html:div
       `("$\\displaystyle{\\frac{\n"
	 ,delimited-premises "}{\n"
	 ,concl "}}$"
	 ,(html:span :class "rname" "(" name ")")))))))

(define (mv base suffix) ;; formatting metavariables
  (if (equal? suffix "")
      #`"\\textcolor{brown}{\\mathbf{,base}}"
      #`"\\textcolor{brown}{\\mathbf{,|base|_,suffix}}"))

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
 -->"))

(define (rnameref rn) ; link to call Show_Stuff
  (html:a :href #`"javascript:Show_Stuff(,(normalize-rname rn))" rn))


;;; game specific functions follow

;; Game nat
(define (EvalTo e n)
  `(,e "\\Downarrow" ,n "\n"))

(define (PlusIs n1 n2 n3)
  `(,n1 "\\mbox{ plus }" ,n2 "\\mbox{ is }" ,n3 "\n"))

(define (MultIs n1 n2 n3)
  `(,n1 "\\mbox{ mult }" ,n2 "\\mbox{ is }" ,n3 "\n"))

(define (STerm n)
  `("S(" ,n ")"))

(define (ZTerm) "Z")

(define (PTerm e1 e2)
  `(,e1 "+" ,e2))

(define (MTerm e1 e2)
  `(,e1 "*" ,e2))


