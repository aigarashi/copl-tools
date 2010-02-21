#! /usr/bin/gosh
;-*-Scheme-*-

(use www.cgi)
(use text.html-lite)
(use rfc.cookie)
(use util.list)
(use file.util)
(use srfi-13)

(load "./site-local.scm")
(load "./global.scm")
(load "./userdb.scm")
(load "./questions.scm")
(load "./process.cgi")

(define-constant thisurl "index.cgi")

(define header 
  (html:head 
   (html:title "「ソフトウェア基礎論」演習システム")
   (html:meta 
    :http-equiv "content-type" 
    :content "text/html; charset=utf-8")
   (html:link :href "./global.css" :rel "stylesheet" :type "text/css")))

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
  (let ((x (or ;; first look up the user's db file
               ;(and (file-exists? (dbname name)) (lookupdb name 'passwd)) 
	       ;; or look up the password file
	       (assoc name *passwd*))))
    (and (pair? x)
	 (equal? (sys-crypt passwd (cdr x)) 
		 (cdr x)))))

(define (display-login-page msg)
  (html:div 
   :id "login"
   (html:h1 "「ソフトウェア基礎論」演習システム(PPL2010版)")
   (html:form 
    :action thisurl :method "post"
    (html:fieldset
     (html:legend "ログインしてください")
     (html:label :for "username" :class "label" "ユーザ名")
     (html:input :type "text" :name "name" :id "username")
     (html:label :for "passwd"  :class "label" "パスワード")
     (html:input :type "password" :name "passwd" :id "passwd")
     (html:input :type "hidden" :name "command" :value "login")
     (if msg
	 (html:p (html:span :class "warn" msg))
	 '())
     (html:input :type "submit" :value "ログイン")))))

(define (display-news)
  (file->string *news* :if-does-not-exist #f))

(define (display-sidebar name)
  (let* ((solved (cdr (lookupdb name 'solved)))
	 (no-q (how-many-q)))
    (html:div
     :id "side"
     (html:h2 name "さんの成績")
     (html:p no-q " 問中 " (length solved) " 問正解 ")
     (display-qlist solved)
     (html:p :id "commandlist"
	     (html:a :href "index.cgi" "おすなば")
	     " | "
	     (html:a :href "?command=logout" "ログアウト"))
     )))

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
		   (html:input :type "submit" :value label))))
	       (rulesurl (html:a 
			  :href (string-concatenate (list "games/" 
							  (symbol->string game) ".html")) 
			  game)))
	  (list
	   (html:p
	    (if (< 1 n) 
		(list (html:a :href #`"?qno=,(- n 1)" "<< 前の問題へ")
		      " ")
		"")
	    (if (< n (how-many-q))
		(html:a :href #`"?qno=,(+ n 1)" "次の問題へ >>")
		""))
	   (html:h1 "第" n "問")
	   (html:p "導出システム " rulesurl " で判断 "
		   (html:pre :id "question" goal)
		   " を導出せよ．")
	   (html:h1 "解答欄")
	   (html:form :enctype "multipart/form-data"
		      :action "index.cgi" :method "post"
		      (html:label "解答ファイル:")
		      (html:input :type "file" :name "derivation")
		      (html:input :type "hidden" :name "command" :value "answer")
		      (commonform "ファイルを送信"))
	   (html:form :action "index.cgi" :method "post"
	    (html:textarea :name "derivation" :rows "25" :cols "100" :wrap "off" goal)
	    (html:input :type "hidden" :name "command" :value "answer")
	    (commonform "フォームの解答を送信"))))
	;; if n is too large
	(html:p "そんな問題はありません"))))

(define (display-sandbox)
  (list
   (html:h1 "おすなば")
   (html:form 
    :name "sandbox" :action "index.cgi" :method "post"
    (html:p "問題を解かずに遊ぶこともできます")
    (html:p "導出システム名を選んでください: "
	    (html:select :name "game"
			 (map (lambda (g) (html:option :value g g)) *gamelist*)))
    (html:textarea :name "derivation" :rows "25" :cols "80" :wrap "off"
		   "ここに導出を書いてください")
    (html:input :type "submit" :value "送信")
    (html:input :type "hidden" :name "no" :value "0")
    (html:input :type "hidden" :name "command" :value "answer"))
   (html:script :type "text/javascript" "<!--
  document.sandbox.derivation.select();
//-->")))

(define (check-and-show params)
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
	    header
	    (html:body
	     (html:div 
	      :id "contents"
	     (html:div
	      :id "main"
	      html-msg
	      (if *debug*
		  (list
		   (html:h1 "Debug information:")
		   (html:h2 "Input:")
		   (html:pre (html-escape-string params))
		   (html:h2 "Output:")
		   (html:pre (html-escape-string result)))
		  '())
	      (cond
	       [(and (zero? (car result)) (< 0 no (how-many-q)))
		(display-q (+ no 1))]
	       [(zero? no) (display-sandbox)]
	       [else '()])
	     (display-sidebar uname)))))))
	(list
	 (cgi-header
	  :content-type "text/html; charset=utf-8")
	 (html-doctype)
	 (html:html
	  header
	  (html:body
	   (html:p
	    (cond [(not (or (zero? no) uname))
		   "ログインしてください"]
		  [(not deriv)
		   "空ファイルが送信されたようです"]
		  [(not game)
		   "不正なアクセスです"]))))))))

(define (main params)
   ; possible parameters
   ;  command: {login, logout}
   ;  name: login name
   ;  passwd:  pass word
   ;  loginas:  login name, obtained from cookie, if already logged in
   ;  qno: question number to show
  (let ((command (string->symbol 
		  (or (cgi-get-parameter "command" params) "nop")))
	(lname (cgi-get-parameter "name" params))
	(lpasswd (cgi-get-parameter "passwd" params))
	(name (cgi-get-parameter "loginas" params))
	(qno (cgi-get-parameter "qno" params)))
    (cond 
     [(and (eq? command 'logout) name) ;; trying to logout
      (list ;; deleting cookie
       (cgi-header :cookies
		   (construct-cookie-string 
		    `(("loginas" ,name 
		       :domain ,*domainname*
		       :path ,*system-url-local*
		       :expires ,(- (sys-time) 1) 
		       :max-age 0))))
       (html-doctype)
       (html:html
	(html:head
	 (html:meta :http-equiv "Refresh" :content "0;url=index.cgi"))
	(html:body
	 (html:p "ログアウトしました．"))))]
     [(and (eq? command 'answer) name)  ;; trying to answer a question
      (check-and-show params)]
     [name  ;; he/she has already logged in
      (list
       (cgi-header)
       (html-doctype)
       (html:html
	header
	(html:body
	 (html:div
	  :id "contents"
	 #;(html:div
	  :id "header")
	  (display-news)
	  (html:div
	   :id "main"
	   (if qno
	       (display-q (string->number qno))
	       (display-sandbox)))
	  (display-sidebar name)
	  ))))]
     [(and (eq? command 'login) (check-passwd lname lpasswd))
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
		       :path ,*system-url-local*
		       :expires ,(+ (sys-time) 86400) 
		       :max-age 86400))))
       (html-doctype)
       (html:html
	header
	(html:body
	 (html:div 
	  :id "contents"
	  (html:div 
	   :id "header"
	   (html:p
	    (html:a :href "?command=logout" "ログアウト")))
	  (display-news)
	  (html:div
	   :id "main"
	   (display-sandbox))
	  (display-sidebar lname)))))]
     (else
      (list
       (cgi-header)
       (html-doctype)
       (html:html
	header
	(html:body
	 :id "login-screen"
	 (display-login-page	 
	  (cond
	   [(eq? command 'login)
	    (html:p "ユーザ名もしくはパスワードが違います")]
	   [(eq? command 'logout) 
	   (html:p "ログアウトするには，まずログインしてください")]
	   [else #f]))
	 ;(display-sandbox "ログインしていなくても遊べます")
	 )))))))

(cgi-output-character-encoding "utf-8")
(cgi-main main :merge-cookies #t)
