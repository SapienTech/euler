;; Ordered fractions
(use-modules (euler utils))

(define (prime-gen max-val)
  (let loop ((curr-val 2) (acc '()))
    (if (> curr-val max-val
	   ;(ceiling (sqrt max-val))
	   ) acc
	(loop (1+ curr-val)
	      (if (truth-fold
		   (lambda (prime) (not (zero? (modulo curr-val prime))))
		   acc)
		  (cons curr-val acc)
		  acc)))))

;; TODO: this is sub-optimal, we want to alternate between 1+ and 1-
(define (find-frac-left-of frac max-denom)
  (let loop ((mult 2) (best-frac 0))
    (let ((curr-frac (/ (1- (* mult (numerator frac)))
			(* mult (denominator frac)))))
      (if (< (- frac curr-frac) (/ (expt max-denom 2)))
	  best-frac
	  (loop (1+ mult)
		(if (<= (denominator curr-frac) max-denom)
		    (begin (display curr-frac) (newline)
			   curr-frac)
		    best-frac))))))
