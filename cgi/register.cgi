#! /usr/bin/gosh
;-*-Scheme-*-

(use www.cgi)
(use text.html-lite)
(use util.match)

(load "./site-local.scm")
(load "./global.scm")
(load "./userdb.scm")

(define-constant thisurl "register.cgi")
(define-constant index "index.cgi")

(define header 
  (html:head 
   (html:title "「ソフトウェア基礎論」演習システム Registration page")
   (html:meta 
    :http-equiv "content-type" 
    :content "text/html; charset=utf-8")
   (html:link :href "./global.css" :rel "stylesheet" :type "text/css")))

(define (display-registration-page . options)
  (let-keywords options ((msg #f)
			 (uname "")
			 (fname "")
			 (address "")
			 (address2 ""))
     (html:div
      :id "registration"
      (html:h1 "Registration form")
      (html:form
       :action thisurl :method "post"
       (html:ol
	(html:li
	 (html:label :for "username" :class "label" "User name")
	 (html:input :type "text" :name "name" :size "8"
		     :id "username" :value uname))
	(html:li
	 (html:label :for "fullname" :class "label" "Your full name")
	 (html:input :type "text" :name "fname" :size "20"
		     :id "fullname" :value fname))
	(html:li
	 (html:label :for "address" :class "label" "email address")
	 (html:input :type "text" :name "address" :size "40" 
		     :id "address" :value address))
	(html:li
	 (html:label :for "address2" :class "label""email address (for doublecheck)")
	 (html:input :type "text" :name "address2" :size "40"
		     :id "address2" :value address2)))
       (html:input :type "hidden" :name "command" :value "register")
       (if msg
	   (html:p (html:span :class "warn" msg))
	   '())
       (html:input :type "submit" :value "Register")))))

(define (invalid-name? s)
  (not (#/^[\w]{2,}$/ s)))

(define (invalid-address? s)
  ;; see http://blog.livedoor.jp/dankogai/archives/51189905.html
  ;; see RFC2822 for details.
  (let* ((atom "[a-zA-Z0-9_!#\\$\\%&'*+/=?\^`{}~|\\-]+")
	 (dot_atom #`",|atom|(?:\\.,|atom|)*")
	 (quoted "\"(?:\\\\[^\\r\\n]|[^\\\\\"])*\"")
	 (local #`"(?:,dot_atom|,quoted)")
	 (domain_lit "\\\[(?:\\\\\\S|[\\x21-\\x5a\\x5e-\\x7e])*\\\]")
	 (domain #`"(?:,dot_atom|,domain_lit)")
	 (addr_spec #`"^,|local|\\@,|domain|$"))
    (not ((string->regexp addr_spec) s))))

(define (check-errors conditions)
  (match conditions
	 [() ()]
	 [((error? . msg) . rest)
	  (if (error?) 
	      (cons msg (check-errors rest))
	      (check-errors rest))]
	 ))

(define (main params)
  ; possbile parameters
  ;  name: login name
  ;  address: email address
  ;  fname: full name
  (let ((command (string->symbol 
		  (or (cgi-get-parameter "command" params) "nop")))
	(uname (cgi-get-parameter "name" params))
	(fname (cgi-get-parameter "fname" params))
	(address (cgi-get-parameter "address" params))
	(address2 (cgi-get-parameter "address2" params)))
    (cond 
     [(eq? command 'register)
      (let ((valid?
	     (check-errors
	      (list
	       (cons (lambda () (zero? (string-length uname)))
		     "User name shouldn't be empty")
	       (cons (lambda () (user-exists? uname))
		     "User already exists!")
	       (cons (lambda () (invalid-name? uname)) 
		     "User name should consist of alphabets, numbers, underscore")
	       (cons (lambda () (invalid-address? address))
		     "Invalid e-mail address")
	       (cons (lambda () (not (string=? address address2)))
		     "The two email addresses do not match")
	       (cons (lambda () (zero? (string-length fname)))
		     "Your full name shouldn't be empty")))))
	(if (null? valid?)  ;; all check passed!
	    (begin
	      (create-temporary-account uname fname address)
	      (list
	       (cgi-header)
	       (html-doctype)
	       (html:html
		header
		(html:body
		 (html:h1 "Registration succeeded!")
		 (html:p "Please retrieve your password through the "
			 (html:a :href index "login page"))))))
	    (list
	     (cgi-header)
	     (html-doctype)
	     (html:html
	      header
	      (html:body
	       (cond
		[(eq? command 'register)
		 (display-registration-page
		  :msg
		  (list
		   (html:p "Registration failure for the following reason(s):")
		   (map (lambda (s) (html:p s)) valid?))
		  :uname uname :fname fname :address address :address2 address2)]
		 [else (display-registration-page)]))))))]
     [else
      (list
       (cgi-header)
       (html-doctype)
       (html:html
	header
	(html:body
	 (display-registration-page))))])))

(cgi-output-character-encoding "utf-8")
(cgi-main main)

;  Yes -> request to choose another name, leaving other entries as they are
;  No -> Put the information in the temporary list with a timp stamp and a 
;        new password
;    expiration: two days
;    login procedure should lookup this temporary user list
;    renew procedure should update this list

