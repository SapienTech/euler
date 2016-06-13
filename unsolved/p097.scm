;; Large non-Mersenne primes

(define-module (solved p097))

(use-modules (euler utils)
	     (srfi srfi-1))

(define-public (last-n-digits num n)
  (take-right (number->digits num) n))

;; NOTE: can use mod for last n digits!
(define-public (last-n-digits-fast num n)
  (modulo num (expt 10 n)))
