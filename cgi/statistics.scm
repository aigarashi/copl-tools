#! /usr/bin/gosh

(use text.html-lite)
(use gauche.sequence)

(load "./site-local.scm")
(load "./global.scm")

(define-constant *question-db* ;; database file for questions
  "/home/igarashi/ppl2010/questions.db")

(load "./userdb.scm")

(use file.util)
(use srfi-13)

(define *histgram* (make-vector how-many-q 0))
(define *score-for-q* 40)

(define unames (map car *passwd*))
(define how-many-users (length unames))

(define (accumulate solved)
  ;; takes a list of solved question numbers and update *histgram*
  (map (lambda (i) 
	 (let* ((i (- i 1)) ;; Question numbers are 1-origin.
		(old (vector-ref *histgram* i)))
	   (vector-set! *histgram* i (+ old 1))))
       solved))

(define (score solved)
  (round 
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
	    (let ((howmany (vector-length (cadar qdb))))
	      (cons (list i (caar qdb) howmany)
		    (aux (cdr qdb) (+ i howmany))))))
      (aux (read in) 1))))

(define (add-rank l)  ;; add-rank: l is a list of pairs of string and number
  (define (aux lastscore howmany lastrank l)
    (if (null? l) '()
	(if (= (cdar l) lastscore)  
	    ;; if this person has the same score as the last one
	    (cons (cons 0 (car l)) 
		  (aux lastscore (+ howmany 1) lastrank (cdr l)))
	    (let ((newrank (+ howmany lastrank)))
	      (cons (cons newrank (car l))
		    (aux (cdar l) 1 newrank (cdr l)))))))
  (aux -1 1 0 l))

(define (display-statistics name)
  (let* ((solved-list (map (lambda (uname) 
			     (cons uname (cdr (lookupdb uname 'solved))))
			   unames))
	 (ranked-list (add-rank (sort (map (lambda (x) 
					     (accumulate (cdr x))
					     (cons (car x) (length (cdr x))))
					   solved-list)
				      (lambda (x y)
					(> (cdr x) (cdr y)))))))
    (list
     (html:h2 "ランキング")
     (html:table :id "ranking"
      (html:tr
       (html:th)
       (html:th "ユーザ名")
       (html:th "解答数"))
      (map-with-index (lambda (i x)
			(html:tr :class (if (string=? (cadr x) name) "you"
					    (if (even? i) "even" "odd"))
				 (if (zero? (car x))
				     (html:td :class "rank")
				     (html:td :class "rank" (car x) "位"))
				 (html:td :class "name" (cadr x))
				 (html:td :class "num" (cddr x) "問")))
	   ranked-list))
     (html:h2 "問題ごとの解答者数")
     (html:table :id "graph"
      (map-with-index (lambda (i n)
			(let ((i (+ i 1)))  ;; qno is 1-origin
			  (html:tr
			   (let ((res (assoc i q-section-list)))
			     (if res 
				 (html:td :class "section"
				  :rowspan (number->string (caddr res))
				  (cadr res))
				 ""))
			   (html:td "第" i "問")
			   (html:td (make-string n #\■) 
				    (make-string (- how-many-users n) #\□) 
				    "(" n "人)"))))
		      *histgram*)))))

(define (main args)
  (write (display-statistics)))

  #;(let* ((unames (collect-usernames))
	 (solved-list (map (lambda (uname) 
			     (cons uname (cdr (lookupdb uname 'solved))))
			   unames)))
    (map (lambda (x) (accumulate (cdr x))) solved-list)
    (write *histgram*)
    (write (map (lambda (x) (cons (car x) (length (cdr x)))) solved-list)))

