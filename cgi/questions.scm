;-*-Scheme-*-

(use text.html-lite)
(use gauche.collection)

(define (title-of section)
  (car section))
(define (description-of section)
  (cadr section))
(define (prerequisites-of section)
  (caddr section))
(define (questions-of section)
  (cadddr section))

(define (get-q n)  ;; the number is 1-origin
  (define (aux n qdb)
    (if (null? qdb) #f
	(let* ((qvec (questions-of (car qdb)))
	       (length (vector-length qvec)))
	  (cond [(< n 0) #f]
		[(< n length)
		 (vector-ref qvec n)]
		[else (aux (- n length) (cdr qdb))]))))
  ;; returns #f if n-th problem doesn't exist
  (call-with-input-file *question-db*
    (lambda (in)
      (let ((n (- n 1))
	    (qdb (read in)))
	(aux n qdb)))))

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
	(if (memq q-no solved)
	    (cons (html:li :class "solved"
			   (html:a :href #`"index.cgi?qno=,q-no" "Q" q-no))
		  (inner-loop (+ q-no 1) until))
	    (cons (html:li :class "unsolved"
			   (html:a :href #`"index.cgi?qno=,q-no" "Q" q-no))
		  (inner-loop (+ q-no 1) until)))))
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
		       :onclick "turnOffOthersAndToggle(this);"
		       (description-of (car sections))
		       #`" (,|section-solved|/,q-section)"
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