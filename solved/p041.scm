;; Pandigital primes

(define-module (solved p041))

(use-modules (euler primes)
	     (euler utils))

;; Based on 3 divisibility, we can eliminate 8 and 9 digit primes
(define* (largest-n-pandigital-prime #:optional (primes (erato (expt 10 7))))
  (let lp ([primes (reverse primes)])
    (if (pandigital? (car primes)) (car primes)
	(lp (cdr primes)))))
