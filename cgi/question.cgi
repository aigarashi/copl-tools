#! /usr/bin/gosh

(use www.cgi)
(use text.html-lite)

(load "./global.scm")

(define *style* "
  span.test {
    background-color : #ffcccc;
  }
")

(define (get-q n)  ;; the number is 1-origin
  (call-with-input-file	*question-db*
    (lambda (in)
      (let ((qdb (read in))
	    (n (- n 1)))
	(if (< n (vector-length qdb)) 
	    (vector-ref qdb n)
	    #f)))))

(define (display-q n)
  (let ((q (get-q n)))
    (if q  ;; if n-th problem exists, q must be a list
	(let* ((game (car q))
	       (goal (cadr q))
	       (commonform 
		(list
		 (html:input :type "hidden" :name "game" :value game)
		 (html:input :type "hidden" :name "no" :value n)
		 (html:input :type "hidden" :name "problem" :value goal)
		 (html:input :type "submit" :value "解答を送信"))))
	  (list
	   (html:h1 "第" n "問")
	   (html:p "体系 " game " で判断 " (html:br)
		   "&nbsp; &nbsp;" goal (html:br)
		   " を導出せよ")
	   (html:h1 "解答欄")
	   (html:form :enctype "multipart/form-data"
		      :action "process.cgi" :method "post"
		      (html:label "解答ファイル:")
		      (html:input :type "file" :name "derivation")
		      commonform)
	   (html:form :action "process.cgi" :method "post"
	    (html:textarea :name "derivation" :rows "25" :cols "80"  goal)
	    commonform)))
	;; if n is too large
	(html:p "そんな問題はありません"))))

(cgi-main
  (lambda (params)
    (let ((q (cgi-get-parameter "no" params)))
      (list
       (cgi-header)
       (html-doctype)
       (html:html
	(html:head (html:style :type "text/css" *style*))
	(html:body
	 (if q (display-q (string->number q))
	     (html:p "Error: the question number has to be specified"))))))))

