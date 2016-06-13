;; Coin sums (using dynamic programming)

;; Both top-down and bottom-up approaches shown. bottom up is faster

(define-module (solved p031))

(use-modules (srfi srfi-1))

(define coins (reverse (list 1 2 5 10 20 50 100 200)))


;; Top-down approach w/out memoization
(define (coin-combinations-for-amount-top-down amount coins)
  (define (loop curr-amount curr-coins combinations)
    (append-map (lambda (combination)
		  (map (lambda (sub-combo)
			 (append combination sub-combo))
		       (let ((sub-combos (inner-loop curr-amount curr-coins)))
			 sub-combos)))
		combinations))


  (define (inner-loop curr-amount curr-coins)
    (apply append (filter-map (lambda (coin index)
		     (cond
		      ((> coin curr-amount) #f)
		      ((= coin curr-amount) (list (list coin)))
		      (else (begin
			      (loop (- curr-amount coin)
				    (drop curr-coins index)
				    (list (list coin)))))))
		   curr-coins
		   (iota (length curr-coins)))))
  (loop amount coins '(())))

(define (top-down-w/out-memoization-fast amount coins)
  (let lp ([curr-amount amount] [coins coins])
    (if (<= (length coins) 1) 1
	(let in-lp ([curr-amount curr-amount] [combinations 0])
	  (if (< curr-amount 0) combinations
	      (in-lp
	       (- curr-amount (car coins))
	       (+ combinations (lp curr-amount (cdr coins)))))))))

(define (top-down-w/memoization amount coins)
  (define coin-length (length coins))
  (define cache (make-array #f (1+ amount) (1+ coin-length)))
  (let lp ([curr-amount amount] [coins coins])
    (if (<= (length coins) 1) 1
	(let in-lp ([curr-amount curr-amount] [combinations 0])
	  (if (< curr-amount 0) combinations
	      (in-lp
	       (- curr-amount (car coins))
	       (let* ([cached-val
		       (array-ref cache
				  curr-amount
				  (- coin-length (length coins)))]
			[sub-combinations
			 (if cached-val cached-val
			     (lp curr-amount (cdr coins)))])
		 (array-set! cache
			     (+ combinations sub-combinations) curr-amount
			     (- coin-length (length coins)))
		 (+ combinations sub-combinations)))))))
  (array-ref cache amount (car coins)))
;; Bottom-up approach
;; TODO: finish
(define (coin-combinations-for-amount-bottom-up amount coins)
  (define cache (make-vector (1+ amount) #f))
  (vector-set! cache 0 1)
  )


