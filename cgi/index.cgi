#! /usr/bin/gosh

(use www.cgi)
(use text.html-lite)
(use rfc.cookie)
(use util.list)

(load "./site-local.scm")
(load "./global.scm")
(load "./userdb.scm")

(define-constant thisurl "index.cgi")

(define header 
  (html:head 
   (html:title "ソフトウェア基礎論演習システム")
   (html:meta 
    :http-equiv "content-type" 
    :content "text/html; charset=utf-8")
   (html:style :type "text/css" *style*)))

(define (command-url com . options)
  (string-concatenate
   (list 
    thisurl
    "?command="
    com
    (string-join
     (map (lambda (p) string-join "=") options)
     "&" 'prefix))))

(define (check-passwd name passwd)
  (let ((x (assoc name *passwd*)))
    (and (pair? x) 
	 (equal? (sys-crypt passwd (cdr x)) 
		 (cdr x)))))

(define (display-login-page)
  (list
   (html:form 
    :action thisurl :method "post"
    (html:h1 "ソフトウェア基礎論演習システムへようこそ")
					;(html:p "ログインしてください")
    (html:p "ユーザ名:" (html:input :type "text" :name "name"))
    (html:p "パスワード:" (html:input :type "password" :name "passwd"))
    (html:input :type "hidden" :name "command" :value "login")
    (html:input :type "submit" :value "ログイン"))))
  
(define-constant gamelist
  '("Nat" "NatLt1" "NatLt2" "NatLt3" "NatExp" 
    "ML1" "ML2" "ML3" "ML4" "ML5" "ML6"
    ))

(define (display-sandbox msg)
  (list
   (html:form 
    :name "sandbox" :action "process.cgi" :method "post"
    (html:p msg)
    (html:p "導出システム名を選んでください: "
	    (html:select :name "game"
			 (map (lambda (g) (html:option :value g g)) gamelist))
	    (html:input :type "submit" :value "送信"))
    (html:textarea :name "derivation" :rows "25" :cols "80" :wrap "off"
		   "ここに導出を書いてください")
    (html:input :type "hidden" :name "no" :value 0))
  (html:script :type "text/javascript" "<!--
  document.sandbox.derivation.select();
//-->")))

(define (read-userdb name)
  (call-with-input-file (dbfile name)
    (lambda (in) (if (port? in) (read in) *empty-userdb*))
    :if-does-not-exist #f))

(define (display-menu name)
  (let* ((userinfo (read-userdb name))
	 (solved (cdr (assoc 'solved userinfo)))
	 (no-q (how-many-q)))
    (list
     (html:h1 "ようこそ " name "さん!")
     (html:h2 "未解答問題")
     (display-qlist (pad-numlist (solved->unsolved no-q solved) 0))
     (html:h2 "解答済み問題")
     (display-qlist (pad-numlist solved 0))
     (html:hr))))

(define (how-many-q)  ;; query how many questions are installed
  (call-with-input-file *question-db*
    (lambda (in)
      (vector-length (read in)))))

(define (display-qlist ns)
  (if (null? ns) (html:p "なし")
      (html:table 
       :class "qlist"
       (map (lambda (rows)
	      (html:tr
	       (map (lambda (n)
		      (html:td
		       (if (zero? n)
			   "&nbsp;"
			   (html:a :href #`"question.cgi?no=,n" n)))) rows)))
	    (slices ns 10 #t 0)))))

(define (pad-numlist l padding)
  (define (aux i ns)
    (if (null? ns) '()
	(if (= i (car ns)) 
	    (cons i (aux (+ i 1) (cdr ns)))
	    (cons padding (aux (+ i 1) ns)))))
  (aux 1 l))

(define (solved->unsolved n solved)  
  ;; n: number of questions
  ;; solved: an increasing list of numbers
  (define (aux i ns)
    (if (> i n) '()
	(if (or (null? ns) (not (eq? (car ns) i)))
	    (cons i (aux (+ i 1) ns))
	    (aux (+ i 1) (cdr ns)))))
  (aux 1 solved))

(define (main params)
   ; possible parameters
   ;  command: {login, logout}
   ;  name: login name
   ;  passwd:  pass word
   ;  loginas:  login name, obtained from cookie, if already logged in
  (let ((command (string->symbol 
		  (or (cgi-get-parameter "command" params) "nop")))
	(lname (cgi-get-parameter "name" params))
	(lpasswd (cgi-get-parameter "passwd" params))
	(name (cgi-get-parameter "loginas" params)))
    (cond 
     ((and (eq? command 'logout) name) ;; trying to logout
      (list ;; deleting cookie
       (cgi-header :cookies
		   (construct-cookie-string 
		    `(("loginas" ,name 
		       :domain ,*domainname*
		       :path "/~igarashi/dc/"
		       :expires ,(- (sys-time) 1) 
		       :max-age 0))))
       (html-doctype)
       (html:html
	(html:head
	 (html:meta :http-equiv "Refresh" :content "3;url=index.cgi")
	 (html:style :type "text/css" *style*))
	(html:body
	 (html:p "ログアウトしました．")))))
     (name  ;; he/she has already logged in
      (list
       (cgi-header)
       (html-doctype)
       (html:html
	header
	(html:body
	 (display-menu name)
	 (display-sandbox "問題を解かずに遊ぶこともできます")
	 (html:hr)
	 (html:a :href (string-append thisurl "?command=logout") "ログアウト")))))
     ((and (eq? command 'login) (check-passwd lname lpasswd))
      ;; if new user, create the db and log files
      (unless (file-exists? (dbfile lname))
	      (call-with-output-file (dbfile lname)
		(lambda (out) (write *empty-userdb* out))))
      (unless (file-exists? (logfile lname))
	      (call-with-output-file (logfile lname)
		(lambda (out) )))
      (list
       (cgi-header :cookies
		   (construct-cookie-string 
		    `(("loginas" ,lname 
		       :domain ,*domainname*
		       :path "/~igarashi/dc/"
		       :expires ,(+ (sys-time) 86400) 
		       :max-age 86400))))
       (html-doctype)
       (html:html
	header
	(html:body
	 (html:p "ログインに成功しました!")
	 (display-menu lname)
	 (display-sandbox "問題を解かずに遊ぶこともできます"))
	(html:hr)
	(html:a :href (string-append thisurl "?command=logout") "ログアウト"))))
     (else
      (list
       (cgi-header)
       (html-doctype)
       (html:html
	header
	(html:body
	 (cond
	  ((eq? command 'login)
	   (html:p "ログイン失敗!"))
	  ((eq? command 'logout) 
	   (html:p "ログアウトするには，まずログインしてください"))
	  (else '()))
	 (display-login-page)
	 #;(display-sandbox "ログインしていなくても遊べます"))))))))

(cgi-output-character-encoding "utf-8")
(cgi-main main :merge-cookies #t)
