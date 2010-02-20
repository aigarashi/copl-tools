#! /usr/bin/gosh
;-*-Scheme-*-

;; TODO: add a link to go back to the problem list

(use www.cgi)
(use text.html-lite)
(use srfi-13)

(load "./site-local.scm")
(load "./global.scm")
(load "./questions.scm")

(define *style* "
  span.test {
    background-color : #ffcccc;
  }
")

(define (display-q n)
  (let ((q (get-q n)))
    (if q  ;; if n-th problem exists, q must be a list
	(let* ((game (car q))
	       (goal (cadr q))
	       (commonform 
		(lambda (label)
		  (list
		   (html:input :type "hidden" :name "game" :value game)
		   (html:input :type "hidden" :name "no" :value n)
		   ;#(html:input :type "hidden" :name "problem" :value goal)
		   (html:input :type "submit" :value label))))
	       (rulesurl (html:a 
			  :href (string-concatenate (list "games/" 
							  (symbol->string game) ".html")) 
			  game)))
	  (list
	   (html:h1 "第" n "問")
	   (html:p "導出システム " rulesurl " で判断 " #;(html:br)
		   #;"&nbsp; &nbsp;" (html:pre goal) #;(html:br)
		   " を導出せよ")
	   (html:h1 "解答欄")
	   (html:form :enctype "multipart/form-data"
		      :action "process.cgi" :method "post"
		      (html:label "解答ファイル:")
		      (html:input :type "file" :name "derivation")
		      (commonform "ファイルを送信"))
	   (html:form :action "process.cgi" :method "post"
	    (html:textarea :name "derivation" :rows "25" :cols "80" :wrap "off" goal)
	    (commonform "フォームの解答を送信"))))
	;; if n is too large
	(html:p "そんな問題はありません"))))

(cgi-main
  (lambda (params)
    (let ((q (cgi-get-parameter "no" params)))
      (list
       (cgi-header)
       (html-doctype)
       (html:html
	(html:head
	 (html:meta 
	  :http-equiv "content-type" 
	  :content "text/html; charset=utf-8")
	 (html:style :type "text/css" *style*))
	(html:body
	 (if q (display-q (string->number q))
	     (html:p "Error: the question number has to be specified"))))))))

