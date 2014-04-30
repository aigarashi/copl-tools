#! /usr/bin/gosh
;-*-Scheme-*-

(use www.cgi)
(use text.html-lite)
(use rfc.cookie)
(use util.list)
(use file.util)
(use srfi-1)
(use srfi-13)
(use srfi-19)
(use util.match)

(load "./site-local.scm")
(load "./global.scm")
(load "./userdb.scm")
(load "./questions.scm")
(load "./process.scm")
(load "./statistics.scm")

(define-constant lastcommit (substring "$Date::                           $" 8 33))

(define-constant thisurl "index.cgi")

(define-constant twenty-four-hours 86400)  ;; in seconds

(define-constant header 
  (html:head 
   (html:title (_ "Concepts of Programming Languages (No.24 of Informatics Core Text Series)"))
   (html:meta 
    :http-equiv "content-type" 
    :content "text/html; charset=utf-8")
   (html:link :href "./global.css" :rel "stylesheet" :type "text/css")))

(define-constant bookinfo
  (html:div 
   :id "bookinfo"
   (html:div :id "box"
   (html:h2 (_ "Bibliographic Information"))
   (html:p "("
	   (html:a :href "http://www.saiensu.co.jp/?page=book_details&ISBN=ISBN978-4-7819-1285-1" (_ "SAIENSU-SHA"))
	   ","
	   (html:a :href "http://www.amazon.co.jp/dp/4781912850/" "amazon.co.jp")
	   ","
	   (html:a :href "http://books.rakuten.co.jp/rb/item/11285225/" "楽天books")
	   ")")
   (html:table
    (html:tr
     (html:td :rowspan "6" (html:img :src "../image/CoPL-thumb.jpg" 
				     :align "left" :width "100px" 
				     :alt "cover picture"))
     (html:td (_ "Paperback, 192 pages")))
    (html:tr
     (html:td (_ "Publisher: Saiensu-Sha Co., Ltd.")))
    (html:tr
     (html:td (_ "Published: July, 2011")))
    (html:tr
     (html:td (_ "Price: 1850 yen (+tax)")))
    (html:tr
     (html:td "ISBN: 978-4-7819-1285-1"))
#;    (html:tr
     (html:td 
      (html:a :href "http://www.saiensu.co.jp/?page=book_details&ISBN=ISBN978-4-7819-1285-1&YEAR=2011" "サイエンス社のこの本に関するページ")))
    
    )
   (html:ul
    (html:li "2012年1月14日のジュンク堂書店池袋本店トークセッション"
     (html:a
      :href "http://www.junkudo.co.jp/tenpo/evtalk.html#20120114juikebukuro" 
      "「新春座談会 このコンピュータ書がすごい！ 2012年版〜2011年に出たコンピュータ書ならこれを読め！〜」")
     "にて本書が紹介された"
     (html:a
      :href "http://www.ustream.tv/recorded/19755352/highlight/232691#utm_campaign=t.co&utm_source=232691&utm_medium=social"
      "模様")
     "です．(2012/1/14記)")
    )
   (html:h2 (_ "Supplemental Material"))
   (html:ul 
    (html:li (html:a :href "errata.html" (_ "Errata")))
    (html:li (html:a :href "guide.pdf" (_ "Guide to the E-Learning System")))
    (html:li (html:a :href "chap11.pdf" (_ "Continuations (EvalContML1)")))
    (html:li (html:a :href "chap12.pdf" (_ "First-class Continuations (EvalContML4)")))
    (html:li (html:a :href "chap13.pdf" (_ "References (EvalRefML3)")))
    )
   (html:h2 (_ "Lectures that use this textbook"))
   (html:ul
    (html:li (html:a :href "/~igarashi/class/sem/" (_ "\"Semantics of Computer Programs\", Graduate School of Informatics, Kyoto University (2013-)")))
    (html:li (html:a :href "/~igarashi/class/sf12w/" (_ "\"Foundations of Software\", Graduate School of Informatics, Kyoto University")))
    (html:li (_ "お茶の水女子大学 大学院人間文化創成科学研究科 理学専攻 情報科学コース「言語意味論」(2012年度)"))
    (html:li (_ "お茶の水女子大学 理学部 情報科学科「計算モデル論」(2012年度)"))
    )

   (html:hr)
   (html:p "Updated on " lastcommit)
   (html:p "Copyright 2011 Atsushi Igarashi"))
))

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
  (let-keywords 
   options ((msg #f) (msg2 #f) (uname ""))
   (html:div
    (html:h1 (_ "Concepts of Programming Languages"))
    (html:h3 (_ "(No.24 of Informatics Core Text Series)"))
    (display-news #t)
    bookinfo
    (html:div 
     :id "login"
     (html:h2 (_ "Log in to the E-Learning System"))
     (html:form 
      :action thisurl :method "post"
      (html:fieldset
       ;(html:legend "ログインしてください")
       (html:label :for "username" :class "label" (_ "User Name"))
       (html:input :type "text" :name "name" :id "username" :value uname :class "input")
       (html:label :for "passwd"  :class "label" (_ "Password"))
       (html:input :type "password" :name "passwd" :id "passwd" :class "input")
       (html:input :type "hidden" :name "command" :value "login")
       (html:input :type "submit" :value "ログイン" :class "button")))
     (if msg
	 (html:p (html:span :class "warn" msg))
	 '())
     (html:form 
      :action thisurl :method "post"
      (html:fieldset
       (html:legend (_ "Password Reissue"))
       (html:label :for "username" :class "label" (_ "User Name"))
       (html:input :type "text" :name "name" :id "username" :class "input")
       (html:input :type "hidden" :name "command" :value "renew")
       (html:input :type "submit" :value "新パスワードの送付" :class "button")))
     (if msg2
	 (html:p (html:span :class "warn" msg2))
	 '())
     (html:p
      (html:a :href "registration.cgi"
      (_ "User Registration")))
      )
    #;(html:div
     :id "footer" 
     (html:hr)
     "Copyright 2011 Atsushi Igarashi"))))

(define (expired? date)  ;; date must be 'never or "YYYY/MM/DD"
  (and (string? date)
       (let ((expiration-date
	      (date->time-utc (string->date date "~Y/~m/~d")))
	     (today (current-time)))
	 (time>? today expiration-date))))

(define (format-news newslist public? passed-sections)
  (match newslist
	 [() ()]
	 [((date title p? expire prerequisites content) . rest)
	  (if (or p?  ;; the news itself should be publicized
		  (and (not public?)  ;; displaying private news
		       (prerequisite-satisfied?
			prerequisites passed-sections)))
	      (let ((id (string-concatenate (list "news" date))))
		(cons
		 (let* ((dt (html:dt (html:div :onclick #`"Toggle(',|id|')"
					       (list "(" date ") " title))))
			(dd (html:div :id id :style "display:none;"
				      (html:dd content))))
		   (if (expired? expire)
		       (html:div :class "oldnews" :style "display:none;" dt dd)
		       (list dt dd)))
		 (format-news rest public? passed-sections)))
	      (format-news rest public? passed-sections))]))

(define-constant JStoggle
  (html:script 
   :type "text/javascript"
   "<!--
function Toggle(id) {
　var div = document.getElementById(id);
　switch (div.style.display) {
　　case \"none\":
　　　div.style.display=\"block\";
　　　break;
　　case \"block\":
　　　div.style.display=\"none\";
　　　break;
　}
}
function ToggleOldnews() {
  var children = document.getElementById(\"newslist\").firstChild.childNodes;
  var n = children.length;
  for(i = 0; i < n; i++)
    if (children[i].getAttribute(\"class\") == \"oldnews\")
    　switch (children[i].style.display) {
    　　case \"none\":
    　　　children[i].style.display=\"block\";
    　　　break;
  　　case \"block\":
        children[i].style.display=\"none\";
        break;
     }
}
//-->"))

(define (display-news public? . name)
  (let* ((newslist (call-with-input-file *news*
			 (lambda (in) (if (port? in) (read in) #f))
			 :if-does-not-exist #f))
	 (solved (if (null? name) '()
		     (cdr (lookupdb (car name) 'solved))))
	 (passed-sections
	  (call-with-input-file *question-db*
	    (lambda (in)
	      (let ((sections (read in)))
		(sections-passed sections solved)))))
	 (formatted-news (if newslist
			     (format-news newslist public? passed-sections)
			     '())))
    (if (null? formatted-news)
	'()
	(list
	 (html:div 
	  :class "newsbox"
	  (html:h1 (_ "News"))
	  (html:p (_ "(Click the header to display/hide the content"))
	  (html:p :class "nofloat")
	  (if (not public?)
	      (html:input :type "button" :onclick "ToggleOldnews()"
			  :value (_ "Display/hide old news"))
	      '())
	  JStoggle
	  (html:div
	   :class "news" :id "newslist" 
	   (html:dl formatted-news)))))))

(define (display-sidebar name)
  (let* ((solved (cdr (lookupdb name 'solved))))
    ;; solved should be non #f
    (html:div
     :id "side"
     (html:h2 (format (_ "~a's grade") name))
     (html:p (format (_ "~d questions solved out of ~d questions" how-many-q (length solved))))
     (html:p (_ "Select a problem by clicking the table below"))
     (display-qlist solved)
     (html:h2 (_ "Misc."))
     (html:p :id "commandlist"
	     (html:a :href "index.cgi" (_ "Sandbox"))
	     " | "
	     (html:a :href (command-url "news") (_ "News"))
	     " | "
	     (html:a :href "guide.pdf" (_ "Manual (pdf)"))
	     " | "
	     (html:a :href "rulebook.pdf" (_ "Cheatsheet (pdf)"))
	     " | "
	     (html:a :href (command-url "stats") (_ "Statistics"))
	     " | "
	     (html:a :href (command-url "logout") (_ "Logout")))
     )))

(define (display-q n uname)
  (let ((q (get-q n)))
    (if q ;; if n-th problem exists, q must be a list
	;; then check if you are qualified to solve it
	(if (not (qualified? n (cdr (lookupdb uname 'solved))))
	    (html:p 
	     (format (_ "You cannot solve Q ~d yet.  Please solve other problems first and come back.") 
		     (number->string n)))
	    (let* ((game (car q))
		   (goal (cadr q))
		   (commonform 
		    (lambda (label)
		      (list
		       (html:input :type "hidden" :name "game" :value game)
		       (html:input :type "hidden" :name "no" :value n)
		       (html:input :type "submit" :value label))))
		   (rulesurl (html:a 
			      :target "_blank"
			      :href (string-concatenate (list "games/" 
							      (symbol->string game) ".html")) 
			      game)))
	      (list
	       (html:p
		(if (< 1 n) 
		    (list (html:a :href #`"?qno=,(- n 1)" (_ "<< Next"))
			  " ")
		    "")
		(if (< n how-many-q)
		    (html:a :href #`"?qno=,(+ n 1)" (_ "Previous >>"))
		    ""))
	       (html:h1 (format (_ "Question No.~d") n)
	       (html:p (format (_ "Derive the judgment ~d in system ~d")
			       rulesurl (html:pre :id "question" goal)))
	       (html:h1 (_ "Fill your solution below:"))
	       (html:form :enctype "multipart/form-data"
			  :action "index.cgi" :method "post"
			  (html:label (_ "Solution file:"))
			  (html:input :type "file" :name "derivation")
			  (html:input :type "hidden" :name "command" :value "answer")
			  (commonform (_ "Send file")))
	       (html:form :action "index.cgi" :method "post"
			  (html:textarea :name "derivation" :rows "25" :cols "100" :wrap "off" goal)
			  (html:input :type "hidden" :name "command" :value "answer")
			  (commonform (_ "Send your solution in the form"))))))
	    ;; if n is too large
	(html:p (_ "No such question")))))

(define (display-sandbox)
  (list
   (html:h1 (_ "Sandbox"))
   (html:form 
    :name "sandbox" :action "index.cgi" :method "post"
    (html:p (_ "You can play without solving problems."))
    (html:p (_ "Choose a derivation system: ")
	    (html:select :name "game"
			 (map (lambda (g) (html:option :value g g)) *gamelist*)))
    (html:textarea :name "derivation" :rows "25" :cols "80" :wrap "off"
		   (_ "// Write a derivation here."))
    (html:input :type "submit" :value (_ "Send"))
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
		(display-q (+ no 1) uname)]
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
		   (_ "Please login first.")]
		  [(not deriv)
		   (_ "Empty input?")]
		  [(not game)
		   (_ "Invalid access.")]))))))))

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
	 (html:p (_ "Logged out.")))))]
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
	  (html:div :id "main" (display-news #f name))
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
		    (html:h1 (_ "Statistics"))
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
	       (display-q (string->number qno) name)
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
	  :msg2 (html:p (_ "Sent to the registered address."))))))]
     [(and (eq? command 'login) 
	   (or (check-passwd lname lpasswd)
	       (check-passwd-tmp lname lpasswd)))
      (unless (check-passwd lname lpasswd)
	      ;; create a db file when first-time login
	      (call-with-output-file (dbfile lname)
		(lambda (out) 
		  (let ((userinfo (lookupdb tmp-users lname)))
		    (write (new-userdb userinfo) out))))
	      ;; and delete the entry from the temporary DB
	      (delete-temporary-account lname))
      (unless (file-exists? (logfile lname))
	      (call-with-output-file (logfile lname)
		(lambda (out) )))
      (list
       (cgi-header :cookies
		   (construct-cookie-string 
		    `(("loginas" ,lname 
		       :domain ,*domainname*
		       :path ,*system-url-local*
		       :expires ,(+ (sys-time) twenty-four-hours) 
		       :max-age ,twenty-four-hours))))
       (html-doctype)
       (html:html
	header
	(html:body
	 (html:div 
	  :id "contents"
	  (html:div
	   :id "main"
	   (display-news #f lname)
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
	     :msg (html:p (_ "Invalid user name or pasword."))
	     :uname lname)]
	   [(eq? command 'logout) 
	    (display-login-page
	     :msg (html:p (_ "To log out, please log in first.")))]
	   [else (display-login-page)]))))))))

(cgi-output-character-encoding "utf-8")
(cgi-main main :merge-cookies #t)
