;; Totient maximum

(define-module (solved p069))

(use-modules (srfi srfi-1)
	     (euler primes)
	     (euler utils))

;;; So this proc gives the answer really quickly, but not sure if provable
(define-public (max-n/totient-fast n)
  (let lp ([primes (erato n)] [acc 1])
    (cond
     [(null? primes) acc]
     [(> (* acc (car primes)) n) acc]
     [else
      (lp (cdr primes) (* acc (car primes)))])))

(define (max-n/totient n)
  (fold (lambda (i totient curr-max)
	  (let* ([n/totient (/ i totient)])
	    (if (> n/totient (cdr curr-max))
		(cons i n/totient)
		curr-max)))
	(cons 0 0)
	(iota n 2)
	(totient-in-range n)))

;;; The actual fast way to generate totient numbers...
(define (totients-in-range range)
  (define primes (erato range))
  (define (totient n)
    (let lp ([curr-primes primes] [acc n])
      (cond
       [(null? curr-primes) acc]
       [(> (car curr-primes) n) acc]
       [else
	(lp (cdr curr-primes)
	    (if (zero? (modulo n (car curr-primes)))
		(* acc (- 1 (/ 1 (car curr-primes))))
		acc))])))
  (let lp ([i 1] [acc '()])
    (if (> i range) (drop (reverse acc) 1)
	(lp (1+ i)
	    (cons (totient i) acc)))))
  
  
;;; All the proc below are old attempts
;; Attempting the speed mod
;;; This is cool, but not sure if I gained any speed, it uses much less memory however
(define (relative-primes-range-tt limit)
  (define non-rel-prime-acc (make-vector (1+ limit) '(0 ())))
  (define (update-acc-lp offset i)
    (do [(curr-i (+ i offset) (+ curr-i offset))]
	[(> curr-i limit)]
      (let [(curr-val
	     (vector-ref non-rel-prime-acc curr-i))]
	(vector-set! non-rel-prime-acc
		     curr-i
		     (list (1+ (car curr-val))
			   (if (truth-fold (lambda (val) (= 1 (gcd val i)))
					   (cadr curr-val))
			       ;; Appending since we want small values in front
			       (append (cadr curr-val) (list i))
			       (cadr curr-val)))))))
  (do [(i 2 (1+ i))]
      [(> i limit) non-rel-prime-acc]
    (when (zero? (modulo i 100)) (display i) (newline))
    (let* ([cached-non-rel-primes (cadr (array-ref non-rel-prime-acc i))]
	   [non-rel-primes (if (null? cached-non-rel-primes) (list i)
			       cached-non-rel-primes)])
      (for-each
       (lambda (index)
	 (update-acc-lp index i))
       non-rel-primes))
    (vector-set! non-rel-prime-acc
		 i
		 (car (vector-ref non-rel-prime-acc
				  i)))))

;; This is the faster version that uses more memory
(define (relative-primes-range limit)
  (define non-rel-prime-acc (make-vector (1+ limit) '()))
  (define (update-acc-lp offset i)
    (do [(curr-i (+ i offset) (+ curr-i i))]
	[(> curr-i limit)]
      (let [(curr-i-non-rel-primes
	     (vector-ref non-rel-prime-acc curr-i))]
	(unless (and (not (null? curr-i-non-rel-primes))
		     (= i (car curr-i-non-rel-primes)))
	  (vector-set! non-rel-prime-acc
		       curr-i
		       (cons i (vector-ref non-rel-prime-acc curr-i)))))))
  (do [(i 2 (1+ i))]
      [(> i limit) non-rel-prime-acc]
    (when (zero? (modulo i 100)) (display i) (newline))
    (let ([i-non-rel-primes (cons i (array-ref non-rel-prime-acc i))])
      (for-each
       (lambda (index)
	 (update-acc-lp index i))
       i-non-rel-primes)
      (vector-set! non-rel-prime-acc
		   i
		   (length (vector-ref non-rel-prime-acc
				       i))))))

(define (get-relative-primes n)
  (let loop ((i 1) (acc '()))
    (if (> i n) acc
	(loop (1+ i)
	      (if (= 1 (gcd n i))
		  (cons i acc)
		  acc)))))

(define (n/totient n)
  (/ n (totient n)))

(define (max-n/totient-slow n)
  (fold (lambda (n n/totient curr-max)
	  (if (> n/totient (cadr curr-max))
	      (list n n/totient)
	      curr-max))
	'(0 0)
	(iota (- n 2) 2)
	(map n/totient (iota (- n 2) 2))))


(define (max-n/totient-fast n best)
  (let loop ((i n) (curr-max (list 0 best)))
    (if (<= i 0) curr-max
	(loop (1- i) (update-curr-max curr-max i)))))

(define (update-curr-max curr-max n)
  (let loop ((i 2) (co-primes 1))
    (cond
     ((< (/ n co-primes) (cadr curr-max)) curr-max)
     ((>= i n) (display (list n (exact->inexact( / n co-primes)))) (newline) (list n (/ n co-primes)))
     (else (loop (1+ i)
		 (if (= 1 (gcd n i))
		     (1+ co-primes)
		     co-primes))))))

(define (generate-co-prime-list n)
  (define co-prime-vector (make-vector (1+ n) 0))
  (define (co-prime-recur node)
    (if (> (car node) n) #f
	(begin
	  (let ((m (car node)) (n (cadr node)))
	    (update-vector node)
	    (co-prime-recur (list (- (* 2 m) n) m))
	    (co-prime-recur (list (+ (* 2 m) n) m))
	    (co-prime-recur (list (+ m (* 2 n)) n))))))
  (define (co-prime-iter nodes)
    (cond
     ((null? nodes) #f)
     ((> (car (car nodes)) n) (co-prime-iter (cdr nodes)))
     (else (begin
	     (update-vector (car nodes))
	     (co-prime-iter
	      (append (get-children (car nodes))
		    (cdr nodes)))))))

  (define (co-prime-generator nodes)
    (display (length nodes))
    (newline)
    (if (null? nodes) #f
	(co-prime-generator (node-loop nodes))))
  (define (node-loop nodes)
    (append-map
     (lambda (node)
       (if (> (car node) n) '()
	   (begin
	     (update-vector node)
	     (get-children node))))
     nodes))
  (define (update-vector node)
    (vector-set! co-prime-vector
		 (car node)
		 (1+ (vector-ref co-prime-vector
				 (car node)))))
  (begin
    (co-prime-iter '((2 1) (3 1)))
    (drop (array->list co-prime-vector) 2)))

(define (generate-co-prime-list-1 n)
  (define co-prime-vector (make-vector (1+ n) 0))
  (define (co-prime-iter nodes i)
    (cond
     ((null? nodes) #f)
     ((> (caar nodes) n) (when (zero? (modulo i 1000000)) (display (length nodes)) (newline)) (co-prime-iter (cdr nodes) (1+ i)))
     (else (begin
	     (update-vector (car nodes))
	     (co-prime-iter
	      (append (get-children (car nodes))
		      (cdr nodes))
	      i)))))

  (define (update-vector node)
    (vector-set! co-prime-vector
		 (car node)
		 (1+ (vector-ref co-prime-vector
				 (car node)))))
  (begin
    (co-prime-iter '((2 1) (3 1)) 0)
    (drop (array->list co-prime-vector) 2)))

;; consider doing a single do?
(define (generate-co-prime-list-sieve n)
  (let ([sieve (make-vector (1+ n) 0)])
    (do ([i 1 (1+ i)])
	([> i n])
      (do ([j (* 2 i) (+ i j)])
	  ([> j n])
	(vector-set! sieve j
		     (1+ (vector-ref sieve j)))))
     (drop (map - (iota (vector-length sieve)) (vector->list sieve)) 2)))

(define (get-children node)
  (let ((m (car node)) (n (cadr node)))
    (list
     (list (- (* 2 m) n) m)
     (list (+ (* 2 m) n) m)
     (list (+ m (* 2 n)) n))))

(define (get-n/totient n co-primes)
  (/ n (totient co-primes)))
