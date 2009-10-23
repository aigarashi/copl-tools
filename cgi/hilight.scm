;-*-Scheme-*-

(use text.html-lite)
(use srfi-13)

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

(define (emphasize s lc1 lc2)
   (let ((lines (string-split s #[\n\r])))
     (string-join
      (insert-tag 
       (car lc1) (cadr lc1) "<span class=\"error\">"
       (if (pair? lc2)
	   (insert-tag (car lc2) (cadr lc2) "</span>" lines)
	   (insert-tag (car lc1) #f "</span>" lines)))
      "\n" 'suffix)))

(define *sample-str* 
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
