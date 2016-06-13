;; Spiral primes

;; Generate erato sieve bitvector
;; We need a function that can go through this vector and determine what is on the diagonal.
;; Recursive function to find diagonals:
;; i = 1 , val = 1 -> i = 2 -> val = 4

(define-module (solved p058))

(use-modules (euler primes))

;; Using miller primality test since we need to check large primes
(define (side-length-of-spiral-when-n-percent-of-diagonals-are-prime n)
  (let lp ([side 4] [i 9] [pass 3] [tests 5])
    (if (< (/ pass tests) n) (begin (display i) (newline) (1- side))
	(lp (+ 2 side) (+ i(* side 4))
	    (let side-lp ([i (+ i side)] [diagonals 3] [pass pass])
	      (if (zero? diagonals) pass
		  (side-lp (+ i side) (1- diagonals)
			   (if (prime? i) (1+ pass) pass))))
	    (+ 4 tests)))))
