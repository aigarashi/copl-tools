#!/usr/bin/gosh

(use www.cgi)
;(use gauche.fcntl)
(use util.match)
(use gauche.logger)

(define-constant FILE_LOCK_TIMEOUT 600)

(define *empty-userdb* '((solved) (passwd)))

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
  (match alist
	 [() (list (cons key content))]
	 [((and (key2 . _) p) . rest)
	  (if (eq? key key2) (cons (cons key content) rest)
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

(define (read-userdb name)   ;; obsolete function
  (call-with-input-file (dbfile name)
    (lambda (in) (if (port? in) (read in) *empty-userdb*))
    :if-does-not-exist #f))

(define (lookupdb uname key)
  (let* ((dbname (dbfile uname))
	 (in (open-input-file dbname :if-does-not-exist #f))
	 (db (if (port? in) (read in) *empty-userdb*))
	 (entry (assoc key db)))
    (when (port? in) (close-input-port in))
    entry))

(define (updatedb uname key proc)
  ;; takes a procedure to convert an old entry to a new one
  ;; #f will be fed to proc when the entry named key isn't found
  (let ((l (lockfile uname)))
    (dynamic-wind
	(lambda () (lock-file l))
	(lambda ()
	  (let* ((dbname (dbfile uname))
		 (in (open-input-file dbname :if-does-not-exist #f))
		 (db (if (port? in) (read in) *empty-userdb*))
		 (entry (assoc key db))
		 (oldentry (if entry (cdr entry) #f)))
	    (receive (out tempfile) (sys-mkstemp dbname)
		     (dynamic-wind
			 (lambda ())
			 (lambda ()
			   (write (update-alist key (proc oldentry) db)
				  out)
			   (sys-rename tempfile dbname)
			   (sys-chmod dbname (string->number "644" 8)))
			 (lambda ()
			   (close-output-port out)
			   (when (port? in) (close-input-port in)))))))
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