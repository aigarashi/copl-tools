#!/usr/bin/gosh
;-*-Scheme-*-

(use www.cgi)
(use text.html-lite)
(use gauche.process)

(load "./site-local.scm")
(load "./global.scm")
(load "./userdb.scm")
(load "./hilight.scm")
(load "./questions.scm")

(define (make-cmd game fullp)
    (if fullp 
	#`",|*system-dir*|checker -full -game ,game 2>&1"
	#`",|*system-dir*|checker -game ,game 2>&1"))
	
(define (invoke-checker game fullp deriv problem)
  (let* ((cmd (append
	       (list #`",|*system-dir*|checker" "-check" deriv "-game" game)
	       (if fullp '("-full") '())
	       (if problem (list "-against" (cadr problem)) '())
	       (list :output :pipe :error :pipe)))
	 ;; An old syntax is used to specify the command line
	 ;; If a newer gauche is run on the system, 
	 ;;   it should be changed (remove apply and move keyword lists
	 (process (apply run-process cmd))
	 (result (port->string (process-output process)))
	 (errmsg (port->string (process-error process))))
    (process-wait process)
    (list (process-exit-status process) result errmsg)))

(define (parse-errmsg s)
  (let ((m1 (#/line\s([0-9]+),\scharacter\s([0-9]+) -- line\s([0-9]+),\scharacter\s([0-9]+)/ s))
	(m2 (#/line\s([0-9]+),\scharacter\s([0-9]+)/ s)))
    (if m1 (cons (list (string->number (m1 1)) (string->number (m1 2)))
		 (list (string->number (m1 3)) (string->number (m1 4))))
	(if m2 (cons (list (string->number (m2 1)) (string->number (m2 2)))
		     '())
	      #f))))

(define (display-result result deriv no uname game)
  (if (zero? (car result))  ;; if the process return code is 0
      ;; 正解!
      (begin
	;; recording the result
	(write-log uname 
		   (format "check succeeded in game ~a" game) :header #t)
	(unless (zero? no)  
		;; if no <> 0, then it must be the case that uname is set
		(update-solved uname no)
		(write-log uname (format "Q #~d solved!" no) :header #t))
	(write-log uname "--")
	(list 
	 (html:h1 (if (zero? no) "正しい導出です．" "正解です．"))))
      ;; 不正解 orz
      (begin
	(unless (zero? no)
		;; if no <> 0, then it must be the case that uname is set
		(write-log uname (format "Solving Q #~d failed!" no) :header #t))
	(write-log uname 
		   (format "check failed in game ~a\n~a" game (caddr result))
		   :header #t)
	(write-log uname "--")
	(list
	 (html:h1 "残念...")
	 (html:pre (html-escape-string (caddr result))) 
	 (html:pre :id "userinput"
		   (let ((lc (parse-errmsg (caddr result))))
		     (if lc
			 (emphasize deriv (car lc) (cdr lc))
			 deriv)))
	 (if (zero? no)
	     '()
	     (list
	      (html:hr)
	      (html:a :href "" :onclick "history.back(); return false"
		      "今の問題にもどる")))
		#;(html:a :href "index.cgi" "あきらめて別の問題を解く")))))

#;(cgi-main
 (lambda (params)
   (let* ((game (cgi-get-parameter "game" params))
	  (fullp (cgi-get-parameter "full" params))
	  (deriv (cgi-get-parameter "derivation" params))
	  (uname (cgi-get-parameter "loginas" params))
	  (no (string->number (or (cgi-get-parameter "no" params) "0")))
	  (problem (get-q no)))
     (if (and game deriv (or (zero? no) uname))
	 (let* ((result (invoke-checker game fullp deriv problem))
		(html-msg (display-result result deriv no uname game)))
	   ;; logging
	   (write-log uname deriv)
	   (write-log uname (make-string 70 #\-))
	   (list
	    (cgi-header
	    :content-type "text/html; charset=utf-8")
	    (html-doctype)
	    (html:html
	     (html:head 
	      (html:meta 
	       :http-equiv "content-type" 
	       :content "text/html; charset=utf-8")
	      (html:link :href "./global.css" :rel "stylesheet" 
			 :type "text/css"))
	     (html:body
	      html-msg
	      (if *debug*
		  (list
		   (html:h1 "Debug information:")
		   (html:h2 "Input:")
		   (html:pre (html-escape-string (list params (cdr problem))))
		   (html:h2 "Output:")
		   (html:pre (html-escape-string result)))
		  '())
	      ))))
	 (list
	  (cgi-header
	    :content-type "text/html; charset=utf-8")
	  (html-doctype)
	  (html:html
	   (html:head 
	    (html:meta 
	     :http-equiv "content-type" 
	     :content "text/html; charset=utf-8")
	    (html:style :type "text/css" *style*))
	   (html:body
	    (html:p
	     (cond [(not (or (zero? no) uname))
		    "ログインしてください"]
		   [(not deriv)
		    "空ファイルが送信されたようです"]
		   [(not game)
		    "このURLを直接アクセスしないでください"]))))))))
 :merge-cookies #t)