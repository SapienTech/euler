;; Quadratic primes
(define-module (solved p027))

(use-modules (euler prmes))

(define (quadratic n a b)
  (+ (* n n) (* a n) b))

(define (consecutive-primes a b)
  (let lp ([n 0])
    (if (prime? (quadratic n a b))
	(lp (1+ n))
	n)))

(define* (max-consecutive-primes a-range #:optional (b-range a-range))
  (let lp ([a (* -1 a-range)] [b (* -1 b-range)] [max-ab (list 0 0 0)])
    (cond
     [(>= a a-range) max-ab]
     [(>= b b-range) (lp (1+ a) (* -1 b-range) max-ab)]
     [else (lp a
	       (1+ b)
	       (let ([consec-primes (consecutive-primes a b)])
		 (if (> consec-primes (car max-ab))
		     (list consec-primes a b)
		     max-ab)))])))
