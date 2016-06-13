;; Square digit-chains
(use-modules (srfi srfi-1)
	     (euler utils))

(define chain-cache #f)

(define (digit-square-sum n)
  (reduce + 0
	  (map (lambda (digit)
		 (expt digit 2))
	       (number->digits n))))

(define (get-chain->loop-val n)
  (let while-loop ([curr-val n])
    (let ([cached-val? (vector-ref chain-cache curr-val)])
      (cond
       [cached-val? cached-val?]
       [(memq curr-val '(1 89)) curr-val]
       [else
	(while-loop (digit-square-sum curr-val))]))))

;; Note: this procedure only works when end > 600
(define (get-chain->loop-vals end)
  (set! chain-cache (make-vector (1+ end) #f))
  (let for-loop ([i 1] [acc '()])
    (when (zero? (modulo i 100000)) (display i) (newline))
    (if (>= i end) acc
	(for-loop (1+ i)
		  (let ([end-val (get-chain->loop-val i)])
		    (vector-set! chain-cache i end-val)
		    (cons (get-chain->loop-val i)
			  acc))))))

(define (solve)
  (fold (lambda (val acc) (+ (if (= val 89) 1 0) acc))
	0
	(get-chain->loop-vals (expt 10 7))))
