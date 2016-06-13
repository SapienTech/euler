(define-module (solved p095))

(use-modules (euler math))

;;; Easiest way to do this: 1->n loop, continue until we get a chain or a value goes above 1 mil
;;; Let's not worry about a cache in case it's not needed.
;;; Why don't we do a quick test...
;;; If we don't use it: it takes o(n^2) to find divisors
;;; o(n^3) to loop through each value
;;; A cache is only helping us with a constant time amount of recalculation...
;;; In conclusion, it's probably not useful to use a cache.

;;; Testing time to find the divisors of a million values
;;; Okay this is too slow, so we need a better way of finding divisors...
;;; Would it make sense to compute all the divisors first, and then do the computation?
;;; That would help separate concerns for sure....o

(define (proper-div-sum divisors)
  (apply + divisors))

;; Will simply loop through all the proper divisors and get the chain
(define (find-largest-amicable-chain limit)
  (define proper-div-lst (proper-divisors-in-range limit))
  ;; I wonder which loop looks better: doing let beforehand or doing let in the loop...
  (define (get-chain start)
    (let lp ([curr-val start] [chain (list start)])
      (let ([next-val
	     (proper-div-sum (array-ref proper-div-lst curr-val))])
	(cond
	 [(> next-val limit) '()]
	 [(= start next-val) chain]
	 [(memq next-val chain) '()]
	 [else (lp next-val
		   (cons next-val chain))]))))
  (let lp ([i 1] [max-chain '()])
    (if (> i limit) max-chain
	(lp (1+ i)
	    (let ([curr-chain (get-chain i)])
	      (if (> (length curr-chain)
		     (length max-chain))
		  curr-chain
		  max-chain))))))

(define (proc x) (1+ x))
