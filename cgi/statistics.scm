#! /usr/bin/gosh

(use text.html-lite)
(use gauche.sequence)
(use util.match)

(load "./site-local.scm")
(load "./global.scm")

(load "./userdb.scm")

(use file.util)
(use srfi-13)
(use srfi-19)

(define *histgram* (make-vector how-many-q 0))
(define *score-for-q* 50)

(define (accumulate! solved)
  ;; takes a list of solved question numbers and update *histgram*
  (map (lambda (i) 
	 (let* ((i (- i 1)))
	   ;; Question numbers are 1-origin.
	   (when (< i how-many-q)
		 (vector-set! *histgram* i (+ (vector-ref *histgram* i) 1)))))
       solved))

(define (score solved)
	(length solved)
  #;(round 
   (fold (lambda (i sum)
	   (let ((n (vector-ref *histgram* (- i 1))))
	     ;; n stands for # of people who solved i-th question
	     ;; n must be non-zero
	     (+ sum
		(if (zero? n) 0 (/ *score-for-q* n)))))
	 0 solved)))

(define q-section-list
  (call-with-input-file *question-db*
    (lambda (in) 
      (define (aux qdb i)
	(if (null? qdb) '()
	    (let ((howmany (vector-length (questions-of (car qdb)))))
	      (cons (list i (caar qdb) howmany)
		    (aux (cdr qdb) (+ i howmany))))))
      (aux (read in) 1))))

(define (add-rank l)  ;; add-rank: l is a list of pairs of string and number
  (define (aux lastscore howmany lastrank l)
    (match l
      [() '()]
      [((and (_ _ score) entry) . rest)
       (if (= score lastscore)  
	    ;; if this person has the same score as the last one
	   (cons (cons 0 entry) 
		 (aux lastscore (+ howmany 1) lastrank rest))
	    (let ((newrank (+ howmany lastrank)))
	      (cons (cons newrank entry)
		    (aux score 1 newrank rest))))]))
  (aux -1 1 0 l))

(define (add-score entry) ;; entry = (uname solved_1 ... solved_n)
  (list (car entry)
	(length (cdr entry))
	(score (cdr entry))))

(define (display-statistics name)
  (let ((solved (cdr (lookupdb name 'solved))))
    (if (zero? solved)
	(html:p "問題を解いたら見られるようになります")
	(let* ((unames (user-list))
	       (how-many-users (length unames))
	       (solved-list (filter-map 
			     (lambda (uname) 
			       (let ((noq (cdr (lookupdb uname 'solved))))
				 (and (positeve? noq)
				      (cons uname noq))))
			     unames)))
	  (map (lambda (x) (accumulate! (cdr x))) solved-list)
	  (let* ((name-solved-score (map (lambda (x) (add-score x)) solved-list))
		 (ranked-list (add-rank (sort name-solved-score
					      (lambda (x y)
						(> (caddr x) (caddr y)))))))
	    (list
	     (html:h2 "ランキング")
	     (html:table
	      :id "ranking"
	      (html:tr
	       (html:th)
	       (html:th "ユーザ名")
	       (html:th "解答数")
	       (html:th "経過日数")
	       (html:th "修了日"))
	      (map-with-index
	       (match-lambda* 
		[(i (rank nm noq score))
		 (let ((finished (lookupdb nm 'finished)))
		   (html:tr 
		    :class (if (string=? nm name) "you"
			       (if (even? i) "even" "odd"))
		    (if (zero? rank)
			(html:td :class "rank")
			(html:td :class "rank" rank "位"))
		    (html:td :class "name" nm)
		    (html:td :class "num" noq "問")
		    (html:td
		     :class "num"
		     (format "~d 日"
			     (round (/ (- (if finished 
					      (cdr finished) (sys-time))
					  (cdr (lookupdb nm 'user-since)))
				       24 60 60)))
		     (html:td 
		      :class "num"
		      (if finished (date->string 
				    (time-utc->date (seconds->time (cdr finished)))
				    "~1")
			  "")))
		    #;(html:td :class "score" score)))])
	       ranked-list))
	     (html:h2 "問題ごとの解答者数")
	     (html:table
	      :id "graph"
	      (map-with-index
	       (lambda (i n)
		 (let ((i (+ i 1))) ;; qno is 1-origin
		   (if
		    (qualified? i solved)
		    (html:tr
		     (let ((res (assoc i q-section-list)))
		       (if res 
			   (html:td :class "section"
				    :rowspan (number->string (caddr res))
				    (cadr res))
			   ""))
		     (html:td "第" i "問")
		     (let* ((ratio (/ n how-many-users))
			    (max-bar-length 20)
			    (bar-length (round->exact (+ (* ratio max-bar-length) 0.5))))
		       (html:td (make-string bar-length #\■) 
				(make-string (- max-bar-length bar-length) #\□)
				(format "(~d %)" (round->exact (* ratio 100))))))
		    '())))
	       *histgram*))))))))

(define (main args)
  (write (display-statistics)))
