;; Prime pair sets

;; We want a vector of primes with lst of pairs

(define-module (unsolved p060))

(use-modules (euler primes)
	     (euler utils)
	     (srfi srfi-1))

(define-public (n-prime-pair-sets n)
  (define prime-pairs (make-prime-pairs 50000))
  (define (get-pair-set i)
    (let lp ([candidate-set (array-ref prime-pairs i)] [acc (list i)])
      (if (null? candidate-set) acc
	  (let* ([candidate-pairs (array-ref prime-pairs (car candidate-set))]
		 [new-candidates (lset-intersection = candidate-pairs
						    candidate-set)]
		 [acc-intersection (lset-intersection = acc candidate-pairs)])
	    (if (and (equal? acc acc-intersection))
		(lp new-candidates (cons (car candidate-set) acc))
		(lp (cdr candidate-pairs) acc))))))
  (let lp ([i 3])
    (cond
     [(> i 100) '()]
     [(not (array-ref prime-pairs i)) (lp (1+ i))]
     [else
      (let ([pair-set (get-pair-set i)])
	(if (>= (length pair-set) n)
	    pair-set
	    (lp (1+ i))))])))

(define prime-size (expt 10 8))
(define prime-bitvector (erato-bit prime-size))

;; TODO: figure out how to use do instead of traditional loop
(define (make-prime-pairs n)
  (define primes (take-while (lambda (prime) (< prime n))
			     (prime-bitvector->lst prime-bitvector)))
  (define prime-pairs
    (let ([prime-pairs (make-vector (1+ n) #f)])
      (array-index-map! prime-pairs
			(lambda (i)
			  (if (array-ref prime-bitvector i) '() #f)))
      prime-pairs))
  (let lp ([primes1 primes] [primes2 primes])
    (cond
     [(null? primes1) (reverse-pairs prime-pairs)]
     [(null? primes2) (lp (cdr primes1) primes)]
     [else
      (let* ([prime1 (car primes1)] [prime2 (car primes2)]
	     [prepend-num (number-append prime1 prime2)]
	     [postpend-num (number-append prime2 prime1)])
	(when (and
	       (< prepend-num prime-size)
	       (< prepend-num prime-size)
	       (array-ref prime-bitvector
			  prepend-num)
	       (array-ref prime-bitvector
			  postpend-num))
	  (array-set! prime-pairs
		      (cons prime2
			    (array-ref prime-pairs
				       prime1))
		      prime1))
	(lp primes1 (cdr primes2)))])))

(define (reverse-pairs prime-pairs)
  (array-map! prime-pairs
	      (lambda (pairs)
		(if pairs
		    (reverse pairs)
		    pairs))
	      prime-pairs)
  prime-pairs)
