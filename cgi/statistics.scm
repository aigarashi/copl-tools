#! /usr/bin/gosh

(use text.html-lite)
(use gauche.sequence)
(use util.match)

(load "./site-local.scm")
(load "./global.scm")

(load "./userdb.scm")

(use file.util)
(use srfi-1)
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
  (define (generate-ranking ranked-list)
    (html:table
     :id "ranking"
     (html:tr
      (html:th)
      (html:th (_ "User name"))
      (html:th (_ "# of solved problems"))
      (html:th (_ "Days passed"))
      (html:th (_ "Date of completion")))
     (map-with-index
      (match-lambda* 
       [(i (rank nm noq score))
	(let ((finished (lookupdb nm 'finished)))
	  (html:tr 
	   :class (if (string=? nm name) "you"
		      (if (even? i) "even" "odd"))
	   (if (zero? rank)
	       (html:td :class "rank")
	       (html:td :class "rank" (format (_ "No.~D") rank)))
	   (html:td :class "name" nm)
	   (html:td :class "num" (format (_ "~D problems") noq))
	   (html:td
	    :class "num"
	    (format (_ "~d days")
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
      ranked-list)))
  (let* ((solved (cdr (lookupdb name 'solved))))
    (if (zero? (length solved))
	(html:p (_ "Please solve one problem and you can see statistics."))
	(let* ((unames (user-list))
	       (solved-list (filter-map 
			     (lambda (uname) 
			       (let ((solved (cdr (lookupdb uname 'solved))))
				 (and (positive? (length solved))
				      (cons uname solved))))
			     unames))
	       (how-many-users (length solved-list)))
	  (map (lambda (x) (accumulate! (cdr x))) solved-list)
	  (let* ((name-solved-score
		  (map (lambda (x) (add-score x)) solved-list))
		 (sorted-list
		  (sort name-solved-score (lambda (x y) (> (caddr x) (caddr y)))))
		 (ranked-list (add-rank sorted-list)))
	    (list
	     ;; if NAME belongs to a group, then generate another table
	     (let ((group (cdr (lookupdb name 'group))))
	       (if (null? group) (html:h2 (_ "ranking"))
		   (list
		    (html:h2 (_ "Ranking within your group"))
		    (generate-ranking
		     (add-rank
		     (filter 
		      (lambda (entry)
			(eq? (cdr (lookupdb (car entry) 'group)) group))
		      sorted-list))
		     )
		    (html:h2 (_ "Global ranking table")))))
	     ;;
	     (generate-ranking ranked-list)
	     ;;
	     (html:h2 (_ "Percentage of correct answers"))
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
		     (html:td (format "Q. ~D" i))
		     (let* ((ratio (/ n how-many-users))
			    (max-bar-length 20)
			    (bar-length (floor->exact (+ (* ratio max-bar-length) 0.5))))
		       (html:td (make-string bar-length #\■) 
				(make-string (- max-bar-length bar-length) #\□)
				(format "(~d %)" (round->exact (* ratio 100))))))
		    '())))
	       *histgram*))))))))

(define (main args)
  (write (display-statistics)))
