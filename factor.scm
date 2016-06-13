;; Factorization algorithms

(define-module (euler factor))

(use-modules (euler primes))

(define-public (trial-factor n)
  (let lp ([n n]
	   [primes (erato (inexact->exact (ceiling (sqrt n))))]
	   [factors '()])
    (cond
     [(null? primes) (cons n factors)]
     [(> (expt (car primes) 2) n) (cons n factors)]
     [(zero? (modulo n (car primes)))
      (lp (/ n (car primes)) primes (cons (car primes) factors))]
     [else
      (lp n (cdr primes) factors)])))


