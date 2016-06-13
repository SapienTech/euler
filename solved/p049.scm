;; Prime permutations
(define-module (solved p049))

(use-modules (euler utils)
	     (euler primes))

(define (n-digit-increasing-prime-sequence-t n)
  (define primes-bit (erato-bit (expt 10 n)))
  (define primes (filter (n-digit-number? n) (prime-bitvector->lst primes-bit)))
  (let lp ([curr-diff 1] [primes primes] [acc '()])
    (cond
     [(null? primes) acc]
     [(> (+ (car primes) (* 2 curr-diff)) (expt 10 n))
	(lp 1 (cdr primes) acc)]
     [else
      (let seq-lp ([val (+ curr-diff (car primes))]
		   [curr-acc (list (car primes))])
	(cond
	 [(> val (expt 10 n))
	  (lp (1+ curr-diff) primes
	      (if (>= (length curr-acc) 3)
		  (cons curr-acc acc)
		  acc))]
	 [(and (bitvector-ref primes-bit val)
	       (permutation? val (car curr-acc)))
	  (seq-lp (+ val curr-diff) (cons val curr-acc))]
	 [else (lp (1+ curr-diff) primes acc)]))])))

(define (n-digit-increasing-prime-sequence n)
  (let* ([primes-bit (erato-bit (expt 10 n))]
	 [primes (filter (n-digit-number? n) (prime-bitvector->lst primes-bit))])
    (let lp ([curr-diff 1] [primes primes])
      (if (> (+ (car primes) (* 2 curr-diff)) (expt 10 n))
	  (lp 1 (cdr primes))
	  (let seq-lp ([val (car primes)] [acc '()])
	    (cond
	     [(> val (expt 10 n)) acc]
	     [(and (bitvector-ref primes-bit val)
		   (if (null? acc) #t (permutation? val (car acc))))
	      (seq-lp (+ val curr-diff) (cons val acc))]
	     [else (lp (1+ curr-diff) primes)]))))))

(define (n-digit-number? n)
  (lambda (num)
    (= n (length (number->digits num)))))
