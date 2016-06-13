;; Square root convergents

(use-modules (srfi srfi-1)
	     (euler utils))


(define (convergent-fracs-where-num-digits>denom-digits expansions)
  (define generate-frac (make-sqrt-convergent-generator 2))
  (let loop ((i expansions) (curr-frac (generate-frac)) (acc 0))
    (if (zero? i) acc
	(loop (1- i)
	      (generate-frac)
	      (if (digits>? (numerator curr-frac)
			    (denominator curr-frac))
		  (1+ acc)
		  acc)))))

(define (make-sqrt-convergent-generator n)
  (let ((*k* #f))
    (lambda ()
      (call-with-prompt
	  'expand
	(lambda ()
	  (if *k* 
	      (*k* (+ n (/ 1 (abort-to-prompt 'expand))))
	      (+ 1 (/ 1 (abort-to-prompt 'expand)))))
	(lambda (k)
	  (set! *k* k)
	  (k n))))))
