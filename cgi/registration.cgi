#! /usr/bin/gosh
;-*-Scheme-*-

(use www.cgi)
(use text.html-lite)
(use util.match)

(load "./site-local.scm")
(load "./global.scm")
(load "./userdb.scm")

(define-constant thisurl "registration.cgi")
(define-constant index "index.cgi")

;; Registration is done in two steps:
;;  1) Temporary registration
;;  2) Official registration
;;
;; Transition from (1) to (2) is triggered by the first login.
;;
;; If information registered at (1) is wrong, the tmp registration can
;; be canceled right after the tmp registration.

(define header 
  (html:head 
   (html:title (_ "User registration"))
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
      (html:h1 (_ "\"Concepts of Programming Languages\""))
      (html:h1 (_ "E-Learning Systemm User Registration Form"))
      (html:form
       :action thisurl :method "post"
       (html:ol
	(html:li
	 (html:label :for "username" :class "label" (_ "User Name"))
	 (html:input :type "text" :name "name" :size "8"
		     :id "username" :value uname))
	(html:li
	 (html:label :for "fullname" :class "label" (_ "Your Name"))
	 (html:input :type "text" :name "fname" :size "20"
		     :id "fullname" :value fname))
	(html:li
	 (html:label :for "address" :class "label" (_ "E-mail address"))
	 (html:input :type "text" :name "address" :size "40" 
		     :id "address" :value address))
	(html:li
	 (html:label :for "address2" :class "label" (_ "E-mail address (to doublecheck)"))
	 (html:input :type "text" :name "address2" :size "40"
		     :id "address2" :value address2)))
       (html:input :type "hidden" :name "command" :value "register")
       (if msg
	   (html:p (html:span :class "warn" msg))
	   '())
       (html:input :type "submit" :value (_ "Register")))
      (html:p :class "warn" (_ "Your e-mail addresses and names will never be used for other purposes than sending a password or contacting you."))
      (html:p :class "warn" (_ "The statistics will show only your user name."))
      )))

(define (invalid-name? s)
  ;; 英数字(ピリオド・ハイフン含む) 2-8文字
  (not (#/^[A-Za-z0-9\.\-]{2,8}$/ s)))

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
		     (_ "User name was empty!"))
	       (cons (lambda () (user-exists? uname))
		     (_ "A user of this name already exists!"))
	       (cons (lambda () (invalid-name? uname)) 
		     (_ "A user name should be 2-8 alphanumeric (including a hyphen and period) charcters."))
	       (cons (lambda () (invalid-address? address))
		     (_ "Invalid e-mail address."))
	       (cons (lambda () (not (string=? address address2)))
		     (_ "The two addresses don't match"))
	       (cons (lambda () (zero? (string-length fname)))
		     (_ "Empty name"))))))
	(if (null? valid?)
	    ;; all checks passed!
	    (begin
	      (create-temporary-account uname fname address)
	      (renew-passwd uname
			    (cgi-get-metavariable "REMOTE_HOST")
			    (cgi-get-metavariable "REMOTE_ADDR"))
	      (list
	       (cgi-header)
	       (html-doctype)
	       (html:html
		header
		(html:body
		 (html:h1 (_ "Temporary registration successful!"))
		 (html:p (format (_ "A temporary password has been sent to ~D.  Please login to the system from ~D to confirm your registration.
If the e-mail address you gave was wrong, please press the button below to cancel the registration") 
				 address (html:a :href index (_ "the top page"))))
		 (html:form
		  :action thisurl :method "post"
		  (html:input :type "hidden" :name "command" :value "cancel")
		  (html:input :type "hidden" :name "name" :value uname)
		  (html:input :type "submit" :value (_ "Cancel registration")))))))
	    ;; when one of the checks failed...
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
		   (html:p (_ "Registration failed for the following reason."))
		   (map (lambda (s) (html:p s)) valid?))
		  :uname uname :fname fname :address address :address2 address2)]
		 [else (display-registration-page)]))))))]
     [(eq? command 'cancel)
      ;; should delete only temporary users.
      ;; Otherwise, anyone can delete any users!
      (let ((nontmp-uname? (file-exists? (dbfile uname))))
	(unless nontmp-uname?
		(delete-temporary-account uname))
	(list
	 (cgi-header)
	 (html-doctype)
	 (html:html
	  header
	  (html:body
	   (display-registration-page
	    :msg (if nontmp-uname?
		     (_ "Cannot remove confirmed users.")
		     (format (_ "Temporary user ~D has been removed.") uname))))))]
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

