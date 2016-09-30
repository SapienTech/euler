;; Quadratic primes
(define-module (euler solved p027)
  #:export (p027-solve))

(use-modules (euler primes)
             (wak foof-loop))

(define (p027-solve)
  (max-consecutive-primes 1000 1001))

(define (quadratic n a b)
  (+ (* n n) (* a n) b))

(define (consecutive-primes a b)
  (loop ((for n (up-from 0))
         (while (prime? (quadratic n a b))))
        => n))

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
