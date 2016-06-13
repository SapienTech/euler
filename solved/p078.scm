;; Coin partitions
;; Using Euler partition generating function

(define-module (solved p078))
(use-modules (euler macros))

;; Euler recursive generating function
;; Pretty messy looking, would be nice to clean up at some point
(define (make-p)
  (let ([cache (make-hash-table)])
    (hashq-set! cache 0 1)
    (rec p
	 (lambda (n)
	   (let lp ([k 1] [acc 0])
	     (cond
	      [(< n 0) 0]
	      [(hashq-ref cache n) (hashq-ref cache n)]
	      [(> k n) (hashq-set! cache n acc) acc]
	      [else 
	       (lp (1+ k)
		   (+ acc
		      (let* ([index1
			      (- n (* (/ 1 2) k (- (* 3 k) 1)))]
			     [index2
			      (- n (* (/ 1 2) k (+ (* 3 k) 1)))]
			     [p1 (hashq-ref cache index1)]
			     [p2 (hashq-ref cache index2)])
			(* (expt -1 (+ k 1))
			   (+
			    (if p1 p1 (p index1))
			    (if p2 p2 (p index2)))))))]))))))
(define (make-p-mod modulus)
  (let ([cache (make-hash-table modulus)])
    (hashq-set! cache 0 1)
    (rec p
	 (lambda (n)
	   (let lp ([k 1] [acc 0])
	     (cond
	      [(< n 0) 0]
	      [(hashq-ref cache n) (hashq-ref cache n)]
	      [(> k n) (hashq-set! cache n (modulo acc modulus)) (modulo acc modulus)]
	      [else 
	       (lp (1+ k)
		   (+ acc
		      (let* ([index1
			      (- n (* (/ 1 2) k (- (* 3 k) 1)))]
			     [index2
			      (- n (* (/ 1 2) k (+ (* 3 k) 1)))]
			     [p1 (hashq-ref cache index1)]
			     [p2 (hashq-ref cache index2)])
			(* (expt -1 (+ k 1))
			   (+
			    (if p1 p1 (p index1))
			    (if p2 p2 (p index2)))))))]))))))

(define (modulo-n-v v start)
  (let [(p (make-p))]
    (let lp ([i start] [curr-val (p start)])
      (when (zero? (modulo i 1000)) (display i) (newline))
      (if (zero? (modulo curr-val v)) i
	  (lp (1+ i) (p (1+ i)))))))

;; Interesting modulus style saving, not sure if it is faster though
(define (modulo-n-v-mod v start)
  (let [(p (make-p-mod v))]
    (let lp ([i start] [curr-val (p start)])
      (when (zero? (modulo i 1000)) (display i) (newline))
      (if (zero? curr-val) i
	  (lp (1+ i) (p (1+ i)))))))
