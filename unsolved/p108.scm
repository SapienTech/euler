;; Diophantine reciprocals I
;; It seems that there is repetition that I can take advantage of!
;; I don't actually need to go that high, since it seems every prime number starts with three, and then every time I double it, then we are good!

(define-module (unsolved p108))

(define (least-n=>distinct-solutions>k solution-proc k)
  (let lp ([n 2] [curr-soltn (solution-proc 2)])
    (if (> curr-soltn k) n
	(lp (1+ n) (solution-proc (1+ n))))))

(define (distinct-diophantine-reciprocals n)
  (let lp ([i (1+ n)] [max-denom (inf)] [acc 0])
    (if (> i max-denom)acc
	(let ([val (diophantine-diff i n)])
	  ; (display (/ 1 i))
	  ; (display " ")
	  ; (display val)
	  ; (newline)
	  (if (= 1 (numerator val))
	      (lp (1+ i)
		  (if (< (denominator val) max-denom)
		      (denominator val) max-denom)
		  (1+ acc))
	      (lp (1+ i) max-denom acc))))))

(define (diophantine-diff x n)
  (- (/ 1 n) (/ 1 x)))
