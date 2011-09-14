;-*-Scheme-*-

(use text.html-lite)
(use srfi-13)
(use gauche.sequence)
(use util.match)

(define (insert-tag-at n tag str)
  (if (number? n)
      (if (> n (string-length str)) 
	  str
	  (let ((segment1 (string-copy str 0 n))
		(segment2 (string-copy str n)))
	    (string-concatenate (list segment1 tag segment2))))
      (string-concatenate (list str " " tag))))

(define (insert-tag line char tag str-list)
  (if (null? str-list) '()
      (if (= line 1)
	  (cons (insert-tag-at char tag (car str-list))
		(cdr str-list))
	  (cons (car str-list)
		(insert-tag (- line 1) char tag (cdr str-list))))))

(define (sort-locs locs)
  (sort locs
	(match-lambda*
	 ((((line1 char1) . _) ((line2 char2) . _))
	  (or (< line1 line2)
	      (and (= line1 line2)
		   ;; if two locations are on the same line,
		   ;; a right-most one comes first
		   (> char1 char2)))))))

(define (generate-linenums n)
  (define (aux i)
    (if (> i n) '()
	(cons i (cons "\n" (aux (+ i 1))))))
  (aux 1))

(define (emphasize lines locs)
  (define sorted-locs (sort-locs locs))
  (define (aux locs lines)
    (match 
     locs
     [() lines]
     [(((line1 char1) . loc2) . rest)
      (aux rest 
	   (insert-tag 
	    line1 char1 "<span class=\"error\">"
	    (match loc2
		   [() (insert-tag line1 #f "</span>" lines)]
		   [(line2 char2)
		    (define (aux2 i lines)
		      (if (= i line2) 
			  (insert-tag line2 char2 "</span>" lines)
			  (insert-tag (+ i 1) 0 "<span class=\"error\">"
				      (aux2 (+ i 1)
					    (insert-tag i #f "</span>" lines)))))
		    (aux2 line1 lines)
		    ])))]))
  (cons (length lines) 
	(string-join (aux sorted-locs lines) "\n" 'suffix)))

#;(define *sample-str* 
"(S(Z) + S(Z)) * S(S(Z)) evalto S(S(S(S(Z)))) by E-Mult { 
  S(Z) + S(Z) evalto S(S(Z)) by E-Plus {
    S(Z) evalto S(Z) by E-Const {};
    S(Z) evalto S(Z) by E-Const {};
    S(Z) plus S(Z) is S(S(Z)) by P-Succ {
      Z plus S(Z) is S(Z) by P-Zero {}
    }
  };
  S(S(Z)) evalto S(S(Z)) by E-Const {};
  S(S(Z)) mult S(S(Z)) is S(S(S(S(Z)))) by M-Succ {
    S(Z) mult S(S(Z)) is S(S(Z)) by M-Succ {
      Z mult S(S(Z)) is Z by M-Zero {};
      S(S(Z)) plus Z is S(S(Z)) by P-Succ {
        S(Z) plus Z is S(Z) by P-Succ {
          Z plus Z is Z by P-Zero {}
        }
      }
    };
    S(S(Z)) plus S(S(Z)) is S(S(S(S(Z)))) by P-Succ {
      S(Z) plus S(S(Z)) is S(S(S(Z))) by P-Succ {
        Z plus S(S(Z)) is S(S(Z)) by P-Zero {}
      }
    }
  }
}"
)
