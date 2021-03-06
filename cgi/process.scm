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
  (define lines (string-split s "\n"))
  (define (aux lines)
    (if (null? lines) '()
	(rxmatch-if 
	 (#/line\s([0-9]+),\scharacter\s([0-9]+) -- line\s([0-9]+),\scharacter\s([0-9]+)/ (car lines))
	 (#f from-line from-char to-line to-char)
	 (cons (cons (list (string->number from-line) (string->number from-char))
		     (list (string->number to-line) (string->number to-char)))
	       (aux (cdr lines)))
	 (rxmatch-if
	  (#/line\s([0-9]+),\scharacter\s([0-9]+)/ (car lines))
	  (#f at-line at-char)
	  (cons (cons (list (string->number at-line) (string->number at-char)) '())
		(aux (cdr lines)))
	  (aux (cdr lines))))))
  (aux lines))

(define (display-result result deriv no uname game)
  ;; result : (int string int string string)
  (if (zero? (car result))  ;; if the process return code is 0
      ;; 正解!
      (begin
	;; recording the result
	;;
	;; Strictly speaking update-solved and update of 'finished may
	;; have to be executed in a transaction, but the solved
	;; problem list is monotonically increasing, so access between
	;; two accesses won't do any harm, I guess.
	(write-log uname 
		   (format "check succeeded in game ~a" game) :header #t)
	(unless (zero? no)  
		;; if no <> 0, then it must be the case that uname is set
		(update-solved uname no)
		(write-log uname (format "Q #~d solved!" no) :header #t))
	(write-log uname "--")
	(list 
	 (html:h1 
	  (cond
	   [(zero? no) "正しい導出です．"]
	   [(and (not (lookupdb uname 'finished)) ;; not yet finished
		 (= (length (cdr (lookupdb uname 'solved)))
		    how-many-q))
	    ;; when the last problem is solved...
	    (write-log uname (format "All the problems have been solved!") :header #t)
	    (updatedb uname 'finished (lambda (s) (sys-time)))
	    (format "正解です．おめでとうございます！これで ~d 問全て解答完了です．" how-many-q)]
	   [else "正解です．"]))))
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
	 (html:table :id "userinput"
		     (let* ((locs (parse-errmsg (caddr result)))
			    (lines (string-split deriv "\n"))
			    (hilighted-text (emphasize lines locs)))
		     (html:tr
		      (html:td :class "lineno"
			       (html:pre (generate-linenums (car hilighted-text))))
		      (html:td :class "code" (html:pre (cdr hilighted-text))))))
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
