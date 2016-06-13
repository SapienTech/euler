;; Truncable primes
(define-module (solved p037))

(use-modules (euler primes)
	     (euler utils)
	     (srfi srfi-1))

;;; Note: proc not optimized
(define (truncable-primes)
  (define primes (erato (expt 10 6)))
  (define (truncable? n)
    (if (memq n '(2 3 5 7)) #f
	(and 
	 (let l->r-lp ([curr-n (l-truncate n)])
	   (cond
	    [(not curr-n) #t]
	    [(not (prime? curr-n)) #f]
	   [else
	    (l->r-lp (l-truncate curr-n))]))
	 (let r->l-lp ([curr-n (r-truncate n)])
	   (cond
	    [(not curr-n) #t]
	    [(not (prime? curr-n)) #f]
	    [else
	    (r->l-lp (r-truncate curr-n))])))))

  (define (prime? n)
    (memq n primes))


  (let lp ([curr-primes primes] [acc '()])
    (cond
     [(null? curr-primes) (display acc)]
     [(= 11 (length acc)) acc]
     [else
      (lp (cdr curr-primes)
	  (if (truncable? (car curr-primes))
	      (cons (car curr-primes) acc)
	      acc))])))


;; truncate left-most value
(define (l-truncate n)
  (digits->number (drop (number->digits n) 1)))
;; truncate right-most value
(define (r-truncate n)
  (digits->number (drop-right (number->digits n) 1)))
