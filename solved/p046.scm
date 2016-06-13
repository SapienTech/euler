;; Goldenbachs other conjecture

;; TODO: it would be nice to not have to guess how many primes i need using erato
;; I can verify when the value is no longer considered when either the prime or the double is larger than it

(define-module (solved p046))

(use-modules (euler primes)
	     (srfi srfi-1)
	     (ice-9 control))

;; Returns value which disproves goldenbach's conjecture
(define (disprove-conjecture)
  (let*
      ([n (expt 10 4)]
       [prime-bitvector (erato-bit n)]
       [prime-lst (prime-bitvector->lst prime-bitvector)]
       [twice-square-lst (map (lambda (val) (* 2 (expt val 2)))
			      (iota n 1))])
    (let lp ([primes prime-lst] [twice-sqr-lst twice-square-lst])
      (cond
       [(null? primes) #f]
       [(null? twice-sqr-lst)
	(lp (cdr primes) twice-square-lst)]
       [else
	(when (<= (+ (car primes) (car twice-sqr-lst)) n)
	  (bitvector-set! prime-bitvector
			  (+ (car primes) (car twice-sqr-lst))
			  #t))
	(lp primes (cdr twice-sqr-lst))]))
    (let answer-lp ([i 3])
      (cond
       [(> i n) "not enough primes"]
       [(bitvector-ref prime-bitvector i) (answer-lp (+ 2 i))]
       [else i]))))
