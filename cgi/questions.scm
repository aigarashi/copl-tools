(use text.html-lite)
(use gauche.collection)

(define (get-q n)  ;; the number is 1-origin
  (define (aux n qdb)
    (if (null? qdb) #f
	(let* ((qvec (cadar qdb))
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

(define how-many-q  ;; query how many questions are installed
  (call-with-input-file *question-db*
    (lambda (in) 
      (define (aux l)
	(if (null? l) 0
	    (+ (vector-length (cadar l)) (aux (cdr l)))))
      (aux (read in)))))

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
  (define (outer-loop q-no sections)
    (if (null? sections) '()
	(let* ((q-section (vector-length (cadar sections)))
	       (end (+ q-no q-section))
	       (section-solved
		(length (filter (lambda (n)
				  (<= q-no n end)) solved)))
	       (finished? (= section-solved q-section)))
	  (cons (html:li
		 :class (if finished? "finished" "off")
		 :onmouseover "this.className='on'"
		 :onmouseout (if finished? 
				 "this.className='finished'" 
				 "this.className='off'")
		 (caar sections)
		 #`" (,|section-solved|/,q-section)"
		 (html:ul
		   :class "questions"
		   (inner-loop q-no (+ q-no q-section))))
		(outer-loop (+ q-no q-section) (cdr sections))))))
	 
  (call-with-input-file *question-db*
    (lambda (in)
      (html:div
       :id "qlist"
       (html:ul 
	:class "qlist"
	(html:li "問題セクション (正解数/問題数)")
	(outer-loop 1 (read in)))))))

