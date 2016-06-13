;; prime utility functions

(define-module (euler primes))

(use-modules (srfi srfi-1)
	     (rnrs sorting)
	     (euler utils))

;; TODO: make vect smaller, and add page segmentation for larger prime values
(define-public (erato limit)
  (let [(prime-bitvect (erato-bit limit))]
    (prime-bitvector->lst prime-bitvect)))

(define-public (prime-bitvector->lst prime-bitvect)
  (let lp ([i (1- (bitvector-length prime-bitvect))]
	   [primes '()])
    (if (zero? i) primes
	(lp (1- i)
	    (if (bitvector-ref prime-bitvect i)
		(cons i primes)
		primes)))))

(define-public (erato-bit limit)
  (define vect (make-bitvector (1+ limit) #t))
  (bitvector-set! vect 0 #f)
  (bitvector-set! vect 1 #f)
  (do [(p 2 (1+ p))]
      [(> p (sqrt limit))]
    (when (bitvector-ref vect p)
      (do [(mul (+ p p) (+ mul p))]
	  [(> mul limit)]
	(bitvector-set! vect mul #f))))
  vect)

;; Good up to around (expt 10 4)
(define-public (primes-in-range limit)
  (let loop ((curr 3) (primes '(2)))
    (if (> curr limit) (reverse primes)
	(loop (+ 2 curr)
	      (if (truth-and-fold (lambda (prime)
				(not (= 0 (modulo curr prime))))
			      primes)
		  (cons curr primes)
		  primes)))))

(define-public (first-n-primes n)
  (let ([generator (make-prime-generator)])
    (let lp ([i n] [acc '()])
      (if (zero? i) (reverse acc)
	  (lp (1- i) (cons (generator) acc))))))

;; Need to consider difference between append and all that
;; Also might be interested in building a large list first...
(define-public (make-prime-generator)
  (define prime-cache '())
  (lambda ()
    (cond
     [(null? prime-cache) (set! prime-cache '(2)) 2]
     [(= 1 (length prime-cache)) (set! prime-cache '(2 3)) 3]
     [else
      (let lp ([i (+ 2 (last prime-cache))])
	(if (truth-and-fold (lambda (prime)
			  (not (= 0 (modulo i prime))))
			prime-cache)
	    (begin (set! prime-cache (append prime-cache (list i)))
		   i)
	    (lp (+ 2 i))))])))

(define prime-cache #f)

(define-public (prime? n)
  (when (or (not prime-cache)
	    (>= n (bitvector-length prime-cache)))
    (set! prime-cache
      (erato-bit (expt n 2))))
  (if (< n 0) #f
      (bitvector-ref prime-cache n)))

;; Evenutally make prime input optional
(define-public (prime-factors n primes)
  (let loop ((curr n) (primes primes) (prime-factors '()))
    (if (= 1 curr) (reverse prime-factors)
	(if (= 0 (modulo curr (car primes)))
	    (loop (/ curr (car primes))
		  primes
		  (cons (car primes) prime-factors))
	    (loop curr (cdr primes) prime-factors)))))

(define (rad n primes)
  (let loop ((curr n) (primes primes) (prime-factors '()))
    (if (= 1 curr)
      (reduce * 1 (delete-duplicates prime-factors))
      (if (= 0 (modulo curr (car primes)))
        (loop (/ curr (car primes))
              primes
              (cons (car primes) prime-factors))
        (loop curr (cdr primes) prime-factors)))))

(define-public (ordered-radicals limit)
  (let* ((primes (primes-in-range limit))
           (rad-pairs (zip (iota limit 1)
                           (map (lambda (n)
                                  (rad n primes))
                                (iota limit 1)))))
    (list-sort (lambda (p1 p2)
		 (if (= (cadr p1) (cadr p2))
		     (< (car p1) (car p2))
		     (< (cadr p1) (cadr p2))))
               rad-pairs)))


;; Gary-Miller primality test

(define (expm b e m) ; modular exponentiation
  (let loop ((b b) (e e) (x 1))
    (if (zero? e) x
      (loop (modulo (* b b) m) (quotient e 2)
            (if (odd? e) (modulo (* b x) m) x)))))

(define (strong-pseudoprime? n a) ; strong pseudoprime base a
  (let loop ((r 0) (s (- n 1)))
    (if (even? s) (loop (+ r 1) (/ s 2))
      (if (= (expm a s n) 1) #t
        (let loop ((r r) (s s))
          (cond ((zero? r) #f)
                ((= (expm a s n) (- n 1)) #t)
                (else (loop (- r 1) (* s 2)))))))))

;; as set for very high valued primes
(define-public (prime? n)
  (let lp ([as '(2 3 5 7 11)])
    (cond
     [(null? as) #t]
     [(= n (car as)) #t]
     [(not (strong-pseudoprime? n (car as))) #f]
     [else (lp (cdr as))])))

(define-public (prime? n k)
  (let loop ((k k))
    (cond ((zero? k) #t)
          ((not (strong-pseudoprime? n (random (+ 2 (- n 3))))) #f)
          (else (loop (- k 1))))))

(define (primality-test limit)
  (define primes (erato limit))
  (let lp ([primes primes] [t 0] [f 0])
    (cond 
     [(null? primes) (values t f)]
     [(prime? (car primes)) (lp (cdr primes) (1+ t) f)]
     [else (display (car primes)) (newline)
	   (lp (cdr primes) t (1+ f))])))
