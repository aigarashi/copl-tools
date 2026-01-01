;-*-Scheme-*-
(define-module questions
  (use text.html-lite)
  (use gauche.collection)
  (use srfi-27)
  (use global)

  (export
   questions-of
   get-q
   qualified?
   how-many-q
   sections-passed
   prerequisite-satisfied?
   display-qlist
   )
)

(select-module questions)

; Format:
;  QDB = (section_1 ... section_n)
;  section = (name description prerequisites QVec)
;   NAME is a symbol as the section's ID
;   DESCRIPTION is a string
;   PREREQUISITES is a list of symbols that this section depends on.
;     All the sections in this list should be 33% solved.
;     They should already occur in the DB.
;   QVec is a vector of problems
;   PROBLEM = (game judgment)
;     The judgment is usually a string.  If it can be a list of strings,
;     their concatenation will be the question.  Moreover, an element
;     can be a list.  Its head is a format sring and tail is a list of
;     possible variations of the problem.  A variation will be chosen
;     randomly and passed to the format function.
;     For example,
;       "Z plus Z is Z"  -> "Z plus Z is Z"
;       ("Z plus ~s is ~s" ("S(Z)" "S(Z)") ("S(S(Z))" "S(S(Z))"))
;          -> "Z plus S(Z) is S(Z)" or "Z plus S(S(Z)) is S(S(Z))"


(define (title-of section)
  (car section))
(define (description-of section)
  (cadr section))
(define (prerequisites-of section)
  (caddr section))
(define (questions-of section)
  (cadddr section))

(define (get-q n uname)  ;; the number is 1-origin, but ...
  (define (aux n qdb)    ;; here n is 0-origin
    (if (null? qdb) #f
        ;; returns #f if n-th problem doesn't exist
	(let* ((qvec (questions-of (car qdb)))
	       (length (vector-length qvec)))
	  (cond [(< n 0) #f]
		[(< n length) (cadr (vector-ref qvec n))]
		[else ;; retrieve from the next section
                 (aux (- n length) (cdr qdb))]))))
  (define q-in-userdb
    ;; retrieve the cached question from the user DB.
    (let ((qs (lookupdb uname 'questions)))
      (and qs (vector-ref (cdr qs) (- n 1) #f))))
  (if q-in-userdb
      (begin (display (format "Cache hit! ~a\n" q-in-userdb)) q-in-userdb)
      ;; if the questions is not cached
      (call-with-input-file *question-db*
        (lambda (in)
          (let* ((n-1 (- n 1))
	         (qdb (read in))
                 (q (aux n-1 qdb)))
            (cond
             ;; If no variation, return without registering.
             ;; (We could cache it, though)
             [(string? q) q]
             ;; format the question, register it to the user DB, and return q
             [else
              (display q)
              (let ((formatted-q
                     ;; convert ("Z plus ~a is ~a" ("S(Z)" "S(Z)") ("S(S(Z))" "S(S(Z))"))
                     ;; to, e.g., "Z plus S(Z) is S(Z)"
                     (apply format
                            (cons
                             (car q)
                             (let* ((variations (cdr q))
                                    (i (random-integer (length variations))))
                               (list-ref variations i))))))
                (updatedb uname 'questions
                          (lambda (old)
                            (let ;; old should be #f or a vector
                                ((old (or old (make-vector n #f))))
                              (cond [(<= (vector-length old) n-1) ; if old is too short
                                     ;; create a fresh vector by copying from old
                                     (vector-tabulate n
                                                      (lambda (i)
                                                        (if (= i n-1) formatted-q
                                                            (vector-ref old i #f))))]
                                    [else ;; or update the old vector
                                     (vector-set! old n-1 formatted-q)
                                     old]))))
                formatted-q)]))))))

(define (qualified? n solved)
  ;; returns #f if n-th problem doesn't exist
  (call-with-input-file *question-db*
    (lambda (in)
      (define qdb (read in))
      (define (aux n sections)
	(if (null? sections) #f
	    (let* ((qvec (questions-of (car sections)))
		   (length (vector-length qvec)))
	      (cond [(< n 0) #f]
		    [(< n length)
		     ;; this problem belong to this section
		     (prerequisite-satisfied?
		      (prerequisites-of (car sections))
		      (sections-passed qdb solved))]
		    [else (aux (- n length) (cdr sections))]))))
      (aux (- n 1) qdb))))

(define how-many-q  ;; query how many questions are installed
  (call-with-input-file *question-db*
    (lambda (in)
      (define (aux sections)
	(if (null? sections) 0
	    (+ (vector-length (questions-of (car sections)))
	       (aux (cdr sections)))))
      (aux (read in)))))

(define-constant JStoggleQ
  (html:script
   :type "text/javascript"
   "<!--
function toggleQ(elm) {
  var mode = elm.className.split(' ');
  switch (mode[0]) {
    case 'on': elm.className='off ' + mode[1]; break;
    case 'off': elm.className='on ' + mode[1]; break;
    default:
  }
}

function turnOffAll() {
  var menuroot = document.getElementById('qlist').lastChild;
  var children = menuroot.childNodes;
  for (var i = 0; i < children.length; i++) {
    if (children[i].hasAttribute('class')
        && children[i].className.split(' ')[0] == 'on') {
      toggleQ(children[i]);
    }
  }
}

function turnOffOthersAndToggle(elm) {
  if (elm.className.split(' ')[0] == 'off') {
    turnOffAll(); toggleQ(elm);
  } else { turnOffAll(); }
}

function isAnyTurnedOn() {
  var menuroot = document.getElementById('qlist').lastChild;
  var children = menuroot.childNodes;
  for (var i = 0; i < children.length; i++) {
    if (children[i].hasAttribute('class')
        && children[i].className.split(' ')[0] == 'on') {
      return children[i];
    }
  }
  return null;
}
//-->"))

;; answer more than 1/3 questions passed, and proceed to the next stage
(define-constant passing-rate 3)
(define (sections-passed sections solved)
  (define (aux q-no sections)
    (if (null? sections) '()
	(let* ((q-section (vector-length (questions-of (car sections))))
	       (end (+ q-no q-section))
	       (section-solved
		(length (filter (lambda (n)
				  (<= q-no n (- end 1))) solved)))
	       (passed? (>= (* passing-rate section-solved) q-section)))
	  (if passed? (cons (title-of (car sections))
			    (aux end (cdr sections)))
	      (aux end (cdr sections))))))
  (aux 1 sections))

(define (prerequisite-satisfied? prerequisites passed)
  ;; Is PREREQUISITES a subset of PASSED?
  (or (null? prerequisites)
      (and (memq (car prerequisites) passed)
	   (prerequisite-satisfied? (cdr prerequisites) passed))))

(define (display-qlist solved)
  ;; solved is a sorted list of numbers of solved questions
  (define (inner-loop q-no until)
    (if (= q-no until) '()
	(cons (html:li :class (if (memq q-no solved) "solved" "unsolved")
		       (html:a :href #`"index.cgi?qno=,q-no" "Q" q-no))
	      (inner-loop (+ q-no 1) until))))
  (define (outer-loop q-no sections passed)
    (if (null? sections) '()
	(let* ((q-section (vector-length (questions-of (car sections))))
	       (end (+ q-no q-section)))
	  (if (prerequisite-satisfied? (prerequisites-of (car sections))
				       passed)
	      (let* ((section-solved
		      (length (filter (lambda (n)
					(and (<= q-no n) (< n end))) solved)))
		     (finished? (= section-solved q-section)))
		(cons (html:li
		       :class (if finished? "off finished" "off default")
		       (html:span
			:onclick "turnOffOthersAndToggle(this.parentNode);"
			:onmouseover "var x = isAnyTurnedOn(); if (x != null && x != this.parentNode) turnOffOthersAndToggle(this.parentNode);"
			(description-of (car sections))
			#`" (,|section-solved|/,q-section)")
		       (html:ul
			:class "questions"
			(inner-loop q-no end)))
		      (outer-loop end (cdr sections) passed)))
	      (cons (html:li
		     :class "off unsolved"
		     "(？_？)")
		    (outer-loop end (cdr sections) passed))))))

  (call-with-input-file *question-db*
    (lambda (in)
      (let ((sections (read in)))
	(html:div
	 :id "qlist"
	 JStoggleQ
	 (html:ul
	  :class "qlist"
	  (html:li "問題セクション (正解数/問題数)")
	  (outer-loop 1 sections
		      (sections-passed sections solved))))))))
