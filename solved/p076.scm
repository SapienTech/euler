;; Counting summations

(define-module (unsolved p076))

(define-public (counting-summation limit)
  (1- (coin-partitions limit)))

;; Borrowed from p078
(define (coin-partitions n)
  (define (pile-loop pile left partitions)
    (cond
     [(= 1 pile) (1+ partitions)]
     [(zero? left)
      (pile-loop (1- pile) (1+ left) (1+ partitions))]
     [else (let ([next-pile (min left pile)])
	     (pile-loop
	      (1- pile) (1+ left)
	      (pile-loop next-pile (- left next-pile) partitions)))]))

  (pile-loop n 0 0))


;; This is not correct, and not very useful, since the cache isn't helping much
(define (coin-partitions-cache n)
  (define partition-cache (make-vector (1+ n) #f))
  (define (pile-loop pile left partitions)
    (cond
     [(= 1 pile) (1+ partitions)]
     [(zero? left)
      (let ([cached-v? (vector-ref partition-cache pile)])
	(if cached-v? (begin (display "cached: ") (display cached-v?) (+ cached-v? partitions))
	    ;; can i do tail recursion here?
	    (let ([part-for-pile
		   (pile-loop (1- pile) (1+ left) (1+ partitions))])
	      (vector-set! partition-cache pile part-for-pile)
	      part-for-pile)))]
     [else (let ([next-pile (min left pile)])
	     (pile-loop
	      (1- pile) (1+ left)
	      (pile-loop next-pile (- left next-pile) partitions)))]))

  (pile-loop n 0 0))
