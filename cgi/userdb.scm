#!/usr/bin/gosh

(use www.cgi)
;(use gauche.fcntl)
(use util.match)
(use gauche.logger)
(use gauche.process)  ;; for process
(use srfi-13)  ;; for string manipulation
(use srfi-27)  ;; for random-integer
(use file.util)

(define-constant FILE_LOCK_TIMEOUT 600)

(define (new-userdb userinfo)
  ;; userinfo = (timestamp passwd address fname)
  (list
   '(solved)
   (cons 'passwd (passwd-of userinfo))
   (cons 'address (address-of userinfo))
   (cons 'fname (fullname-of userinfo))))

;; temporary user list management
;; format
;;  ((uname_1 timpstamp_1 passwd_1 address_1 fname_1)
;;   ...
;;   (uname_n timestamp_n passwd_n address_n fname_n))

(define-constant tmp-users
  ;; should not be a valid user name
  "@TMP-USERS")  

(define (timestamp-of entry) (cadr entry))
(define (passwd-of entry) (caddr entry))
(define (address-of entry) (cadddr entry))
(define (fullname-of entry) (car (cddddr entry)))

(define (dbfile uname)
  (string-append *userdb-dir* uname ".db"))

(define (logfile uname)
  (string-append *userdb-dir* uname ".log"))

(define (lockfile uname)
  (string-append *userdb-dir* uname ".lock"))

(define (write-log uname msg . options)
  ;; if uname is #f, do nothing
;  (let-keywords options ((:header #f))
  (when uname
      (log-open (logfile uname) :prefix (if (null? options) "" "~T: "))
      (log-format msg)))

(define (update-alist key content alist)
  ; removes the entry named KEY if CONTENT is #f
  (match alist
	 [() (list (cons key content))]
	 [((and (key2 . _) p) . rest)
	  (if (equal? key key2) 
	      (if content
		  (cons (cons key content) rest)
		  rest)
	      (cons p (update-alist key content rest)))]))

;; code to manually lock a file, adapted from gauche.logger
(define (lock-file lock)
;  (sys-fcntl port |F_SETLKW| lock)
  (let ((retry-limit 10))
    (let loop ((retry 0)
	       (o (open-output-file lock :if-exists #f)))
      (cond (o (close-output-port o) #t)
	    ((> retry retry-limit)
	     (error))
	    ((file-mtime<? lock (- (sys-time) FILE_LOCK_TIMEOUT))
	     ;; maybe the lock file is stale.
	     (sys-unlink lock)
	     (loop 0 (open-output-file data :if-exists #f)))
	    (else
	     (sys-sleep 1)
	     (loop (+ retry 1) (open-output-file data :if-exists #f)))))))

(define (unlock-file lock)
;  (slot-set! lock 'type |F_UNLCK|)
;  (sys-fcntl port |F_SETLK| lock)
  (sys-unlink lock))

#;(define (read-userdb name)   ;; obsolete function
  (call-with-input-file (dbfile name)
    (lambda (in) (if (port? in) (read in) *empty-userdb*))
    :if-does-not-exist #f))

(define (lookupdb uname key)
  ;; returns (key . entry) or #f if UNAME doesn't exist or KEY is not found
  (let* ((dbname (dbfile uname))
	 (in (open-input-file dbname :if-does-not-exist #f)))
    (and in
	(let* ((db (read in))
	       (entry (assoc key db)))
	  (when (port? in) (close-input-port in))
	  entry))))

(define (updatedb uname key proc)
  ;; takes a procedure to convert an old entry to a new one
  ;; #f will be fed to proc when the entry named key isn't found
  (let ((l (lockfile uname)))
    (dynamic-wind
	(lambda () (lock-file l))
	(lambda ()
	  (let*
	      ((dbname (dbfile uname))
	       (in (open-input-file dbname :if-does-not-exist #f)))
	    (and
	     in
	     (let* ((db (read in))
		    (entry (assoc key db))
		    (oldentry (if entry (cdr entry) #f)))
	       (receive 
		(out tempfile) (sys-mkstemp dbname)
		(dynamic-wind
		    (lambda ())
		    (lambda ()
		      (write (update-alist key (proc oldentry) db)
			     out)
		      (sys-rename tempfile dbname)
		      (sys-chmod dbname (string->number "644" 8)))
		    (lambda ()
		      (close-output-port out)
		      (when (port? in) (close-input-port in)))))))))
	(lambda () (unlock-file l)))))

(define (update-solved uname new)
  (define (insert m ns)
    ;; insert a nat m into an increasing list ns without duplication
    (match ns
	   [() (list m)]
	   [(hd . tl) (cond ((> m hd) 
			     (cons hd (insert m tl)))
			    ((= m hd) ns)  ;; avoid duplication 
			    (else (cons m ns)))]))
  (updatedb uname 'solved 
	    (lambda (old) 
	      (if old (insert new old) (list new)))))

(define (renew-passwd uname remotehost remoteIP)
  ;; returns #f if uname doesn't exist
  (let ((address-entry (lookupdb uname 'address))
	(tmp-address (lookupdb tmp-users uname)))
    (when (or (and address-entry
		   ;; is address non-empty?
		   (< 0 (string-length (cdr address-entry))))
	      (and tmp-address
		   ;; is this temporary account still valid?
		   (not (expired? (timestamp-of tmp-address)))
		   (< 0 (string-length (address-of tmp-address)))))
	  (let*
	      ((address (or (and address-entry (cdr address-entry)) 
			 (address-of tmp-address)))
	       (process (run-process '(pwgen "-s") :output :pipe))
	       (newpasswd (read-line (process-output process)))
	       (salt (list->string ;; salt is picked up from [A-Z][A-Z].
		      (list
		       (integer->char (+ 65 (random-integer 26)))
		       (integer->char (+ 65 (random-integer 26))))))
	       (hash (sys-crypt newpasswd salt))
	       (msg 
#`"This is a message from the E-learning system for the book \"Concepts of Programming Languages\".
Below is your account information:

    User name: ,|uname|
    Password: ,|newpasswd|

If you are not sure about this message, please contact 
  igarashi@kuis.kyoto-u.ac.jp")
	       (title "[CoPL E-Learning] Your account information")
	       (mail-process (run-process `(mail ,address "-s" ,title) 
				     :input :pipe
				     :error :pipe))
	       (output (process-input mail-process)))
	    (display msg output)
	    (close-output-port output)
	    (if address-entry
		;; if normal-users
		(begin
		  (updatedb uname 'passwd (lambda (old) hash))
		  (write-log uname 
			     (format "Password change request from ~a (~a)"
				     remotehost remoteIP)
			     :header #t))
		(begin
		  ;; if temporary user
		  (updatedb tmp-users uname
			    (match-lambda 
			     ((timestamp _ address fname)
			      (list timestamp hash address fname))))
		  (write-log tmp-users
			     (format "Password change request for tmp user ~a from ~a (~a)"
				     uname remotehost remoteIP))))
	    ))
    (or address-entry tmp-address)))

(define (user-list)
  ;; can exclude temporary users since they have never logged in
  (map (lambda (x) (path-sans-extension x))
       (directory-list *userdb-dir* :children? #t
        :filter (lambda (f) 
		  (and (string-suffix? ".db" f)
		       (not (string=? tmp-users (path-sans-extension f))))))))

(define (check-passwd name passwd)
  (let ((x (lookupdb name 'passwd)))
    (and (pair? x)
	 (let ((hash (cdr x)))
	   (equal? hash (sys-crypt passwd hash))))))

(define (check-passwd-tmp name passwd)
  (let ((x (lookupdb tmp-users name)))
    (and (pair? x)
	 (let ((hash (passwd-of x)))
	   (equal? hash (sys-crypt passwd hash))))))

(define-constant three-days (* 60 60 24 3))
(define (expired? time)
  (> (sys-difftime (sys-time) time) three-days))

(define (user-exists? name) 
  (or (file-exists? (dbfile name))
      (let ((x (lookupdb tmp-users name)))
	(and x
	     (not (expired? (timestamp-of x)))))))

;;
;; Format of temporary DB
;;   DB = (entry_1 ... entry_n)
;;   entry_i = (uname when-created passwd-hash email full-name)
;;
(define (create-temporary-account uname fname address)
  ;; uname : string should be fresh
  (let* ((process (run-process '(pwgen "-s") :output :pipe))
	 (newpasswd (read-line (process-output process)))
	 (salt (list->string ;; salt is picked up from [A-Z][A-Z].
		(list
		 (integer->char (+ 65 (random-integer 26)))
		 (integer->char (+ 65 (random-integer 26))))))
	 (hash (sys-crypt newpasswd salt)))
    (updatedb tmp-users uname
	      (lambda (old)
		(list (sys-time) hash address fname)))))

(define (delete-temporary-account uname)
  ;; deletes the temporary account of UNAME
  ;; the function to compute the new content always returns #f
  ;;  whether or not UNAME already exists
  (updatedb tmp-users uname (lambda (old) #f)))