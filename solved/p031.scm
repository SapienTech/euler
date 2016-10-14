;; Coin sums (using dynamic programming)
;; Both top-down and bottom-up approaches presented (bottom-up significantly faster

(define-module (euler solved p031))
(use-modules (srfi srfi-1)
             (wak foof-loop)
             (ice-9 match))

(define coins '(200 100 50 20 10 5 2 1))

;;; Top-down:

(define* (coin-sums amount #:optional (coins coins))
  (if (<= (length coins) 1) 1
      (loop continue ((amount amount))
            (if (< amount 0) 0
                (+ (continue (- amount (car coins)))
                   (coin-sums amount (cdr coins)))))))

(define* (coin-sums-match amount #:optional (coins coins))
  (match coins
    ((coin) 1) 
    ((coin . rest)
     (loop continue ((amount amount))
           (if (<= amount 0) 1
               (+ (continue (- amount coin))
                  (coin-sums amount rest)))))))

;;; TODO: simplify proc
(define* (top-down-w/memoization amount #:optional (coins coins))
  (define diff-coins-count (length coins))
  (define cache (make-array #f (1+ amount) (1+ diff-coins-count)))
  (let lp ([curr-amount amount] [coins coins])
    (if (<= (length coins) 1) 1
	(let in-lp ([curr-amount curr-amount] [combinations 0])
	  (if (< curr-amount 0) combinations
	      (in-lp
	       (- curr-amount (car coins))
	       (let* ([cached-val
		       (array-ref cache
				  curr-amount
				  (- diff-coins-count (length coins)))]
			[sub-combinations
			 (if cached-val cached-val
			     (lp curr-amount (cdr coins)))])
		 (array-set! cache
			     (+ combinations sub-combinations) curr-amount
			     (- diff-coins-count (length coins)))
		 (+ combinations sub-combinations)))))))
  (array-ref cache amount (car coins)))

;; Bottom-up
;; TODO: finish
(define (coin-combinations-for-amount-bottom-up amount coins)
  (define cache (make-vector (1+ amount) #f))
  (vector-set! cache 0 1))
