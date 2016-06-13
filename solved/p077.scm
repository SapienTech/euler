;; Prime summations

(define-module (solved p077))

(use-modules (euler primes)
	     (srfi srfi-1))

;; NOTE: for some reason gc does not remove prime-lst and prime-lst-generator...
(define (n-prime-sum-number n)
  (define prime-lst '(0))		; Have to make non-empty...
  (define prime-lst-generator! (make-list-generator (make-prime-generator)
						    prime-lst))
  (define node-table (make-hash-table))
  (let lp ([i 1])
    (when (> i (last prime-lst))
      (add-node! node-table (make-prime-sum-node 0 (prime-lst-generator!))))
    (let ([nodes (hashq-ref node-table i)])
      (cond
       [(not nodes) (lp (1+ i))]
       [(> (length nodes) n) i]
       [else
	(for-each (λ (node)
		     (add-next-generation! node-table node))
		  nodes)
	(lp (1+ i))]))))

(define* (make-list-generator item-generator #:optional (lst '(1)))
  (define first-call? #t)
  (λ ()
     (if first-call?
	 (begin (set! first-call? #f)
		(list-set! lst 0 (item-generator))
		lst)
	 (begin
	   (list-cdr-set! lst (1- (length lst)) `(,(item-generator)))
	   lst))))

(define (add-node! table node)
  (let ([prev-nodes (hashq-ref table (get-sum node))])
    (if prev-nodes
	(hashq-set! table (get-sum node)
		    (cons node prev-nodes))
	(hashq-set! table (get-sum node) `(,node)))))

(define (make-prime-sum-node curr-sum primes)
  (list (+ curr-sum (last primes))
	primes))

;; TODO: need better name than parent
(define (get-sum parent) (car parent))

(define (get-primes parent) (cadr parent))

(define (add-next-generation! table parent)
  (let lp ([curr-primes (reverse (get-primes parent))])
    (unless (null? curr-primes)
      (add-node! table (make-prime-sum-node (get-sum parent)
					   (reverse curr-primes)))
      (lp (cdr curr-primes)))))
