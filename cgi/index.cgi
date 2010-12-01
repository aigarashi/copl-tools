#! /usr/bin/gosh
;-*-Scheme-*-

(use www.cgi)
(use text.html-lite)
(use rfc.cookie)
(use util.list)
(use file.util)
(use srfi-13)
(use util.match)

(load "./site-local.scm")
(load "./global.scm")
(load "./userdb.scm")
(load "./questions.scm")
(load "./process.cgi")
(load "./statistics.scm")

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

(define (display-login-page . options)
  (let-keywords options ((msg #f)
			 (uname ""))
   (html:div 
    :id "login"
    (html:h1 "「ソフトウェア基礎論」演習システム")
   (html:form 
    :action thisurl :method "post"
    (html:fieldset
     (html:legend "ログインしてください")
     (html:label :for "username" :class "label" "ユーザ名")
     (html:input :type "text" :name "name" :id "username" :value uname)
     (html:label :for "passwd"  :class "label" "パスワード")
     (html:input :type "password" :name "passwd" :id "passwd")
     (html:input :type "hidden" :name "command" :value "login")
     (html:input :type "submit" :value "ログイン")))
   (html:form 
    :action thisurl :method "post"
    (html:fieldset
     (html:legend "パスワードの(再)発行と送付")
     (html:label :for "username" :class "label" "ユーザ名")
     (html:input :type "text" :name "name" :id "username")
     (html:input :type "hidden" :name "command" :value "renew")
     (html:input :type "submit" :value "新パスワードの送付")))
   (if msg
       (html:p (html:span :class "warn" msg))
       '()))))

(define (format-news newslist)
  (match newslist
	 [() ()]
	 [((date title content) . rest)
	  (let ((id (string-concatenate (list "news" date))))
	    (cons
	     (html:dt (html:div :onclick #`"Toggle(',|id|')"
				(list "(" date ") " title)))
	     (cons
	      (html:div :id id
			:style "display:none;"
			(html:dd content))
	      (format-news rest))))]))

(define-constant JStoggle
  (html:script 
   :type "text/javascript"
   "<!--
function Toggle(id) {
　div = document.getElementById(id);
　switch (div.style.display) {
　　case \"none\":
　　　div.style.display=\"block\";
　　　break;
　　case \"block\":
　　　div.style.display=\"none\";
　　　break;
　}
}
//-->"))

(define (display-news)
  (let* ((newslist  (call-with-input-file *news*
		      (lambda (in) (if (port? in) (read in) #f))
		      :if-does-not-exist '()))
	 (formatted-news (format-news newslist)))
    (if (null? formatted-news)
	'()
	(list
	 (html:h1 "おしらせ")
	 (html:p "内容の表示・非表示を切り替えるには日付をクリックしてください．")
	 JStoggle
	 formatted-news))))

(define (display-sidebar name)
  (let* ((solved (cdr (lookupdb name 'solved))))
    ;; solved should be non #f
    (html:div
     :id "side"
     (html:h2 name "さんの成績")
     (html:p how-many-q " 問中 " (length solved) " 問正解 ")
     (html:p "問題を下の表から選択してください")
     (display-qlist solved)
     (html:h2 "その他")
     (html:p :id "commandlist"
	     (html:a :href "index.cgi" "おすなば")
	     " | "
	     (html:a :href (command-url "news") "おしらせ")
;	     " | "
;	     (html:a :href "rulebook.pdf" "推論規則集(pdf)")
	     " | "
	     (html:a :href (command-url "stats") "統計")
	     " | "
	     (html:a :href (command-url "logout") "ログアウト"))
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
	    (if (< n how-many-q)
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
		   "// ここに導出を書いてください")
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
	       [(and (zero? (car result)) (< 0 no how-many-q))
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
       (cgi-header :content-type "text/html; charset=utf-8"
		   :cookies
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
     [(and (eq? command 'news) name)
      (list
       (cgi-header)
       (html-doctype)
       (html:html
	header
	(html:body
	 (html:div
	  :id "contents"
	  (html:div :id "main" (display-news))
	  (display-sidebar name)))))]
     [(and (eq? command 'stats) name)  ;; show a statistics page
      (list
       (cgi-header)
       (html-doctype)
       (html:html
	header
	(html:body
	 (html:div
	  :id "contents"
	  (html:div :id "main"
		    (html:h1 "統計コーナー")
		    (display-statistics name))
	  (display-sidebar name)))))]
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
	  (html:div
	   :id "main"
	   (if qno
	       (display-q (string->number qno))
	       (display-sandbox)))
	  (display-sidebar name)
	  ))))]
     [(and (eq? command 'renew))
      (renew-passwd lname
		    (cgi-get-metavariable "REMOTE_HOST")
		    (cgi-get-metavariable "REMOTE_ADDR"))
      ;; we don't care whether this user really exists or not
      ;; to discourage someone malicious from renewing passwords repeatedly
      (list
       (cgi-header)
       (html-doctype)
       (html:html
	header
	(html:body
	 :id "login-screen"
	 (display-login-page	 
	  :msg (html:p "Password informaiton has been sent")))))]
     [(and (eq? command 'login) 
	   (or (check-passwd lname lpasswd)
	       (check-passwd-tmp lname lpasswd)))
      (unless (check-passwd lname lpasswd)
	      ;; create a db file when first-time login
	      (call-with-output-file (dbfile lname)
		(lambda (out) 
		  (let ((userinfo (lookupdb tmp-users lname)))
		    (write (new-userdb userinfo) out)))))
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
	   :id "main"
	   (display-news)
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
	  (cond
	   [(eq? command 'login)
	    (display-login-page	
	     :msg (html:p "ユーザ名もしくはパスワードが違います")
	     :uname lname)]
	   [(eq? command 'logout) 
	    (display-login-page
	     :msg (html:p "ログアウトするには，まずログインしてください"))]
	   [else (display-login-page)]))))))))

(cgi-output-character-encoding "utf-8")
(cgi-main main :merge-cookies #t)
