;; Counting rectangles

;; There clearly seems to be an elegant algorithm for this problem...
;; Turning this into a 1-d problem...
;; Given block-size: we have: row-size - size perumations...
;; Then for the 2-d version, I think we just multiply.... since for each block config for 1-d, we have the number of permuations allowed in the the other direction..

(define-module (solved p085))

(define (rectangles-in-area n m)
  (* (/ (* n (+ n 1)) 2)
     (/ (* m (+ m 1)) 2)))

(define (grid-w/~n-rectangles n)
  (let lp ([i 1] [j 1] [best '(0 0 0)])
    (let ([curr-count (rectangles-in-area i j)])
      (cond
       [(and (> curr-count n) (= 1 j)) best]
       [(> j i) (lp (1+ i) 1 best)]
       [(> curr-count n) (lp (1+ i) 1 best)]
       [else
	(lp i (1+ j)
	    (if (< (- n curr-count)
		   (- n (car best)))
		(list curr-count i j)
		best))]))))
