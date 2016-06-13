;; Searching a triangle array for a sub-triangle having minimum-sum

(define-module (unsolved p150))

(use-modules (unsolved p150h)
	     (euler generators)
	     (srfi srfi-1)
	     (rnrs base))

;; Going to pretend that I don't have indexes
;; We probably want to do indices actually, since we need to know history
;; This might be a good macro opportunity
;; Looks like it's faster to do pattern matching
(define-public (min-sub-triangle-sum triangle)
  (define row-length (- (triangle-rows triangle) 1))
  (define side-sums (make-sides triangle))
  (define intersect-sums (make-intersections triangle))
  (let lp ([l-idx 0] [r-idx 0] [b-idx 0] [max-sum 0])
    (cond
     [(> l-idx row-length) (- (triangle-sum triangle) max-sum)]
     [(> r-idx (- row-length l-idx)) (lp (1+ l-idx) 0 0 max-sum)]
     [(> b-idx (- row-length (+ l-idx r-idx))) (lp l-idx (1+ r-idx) 0 max-sum)]
     [else
      (lp
       l-idx r-idx (1+ b-idx)
       (let ([curr-sum
	      (- (+ 
		  (vector-ref (car side-sums) l-idx)
		  (vector-ref (cadr side-sums) r-idx)
		  (vector-ref (last side-sums) b-idx))
		 (+ 
		  (vector-ref (vector-ref (car intersect-sums) l-idx) r-idx)
		  (vector-ref (vector-ref (cadr intersect-sums) l-idx) b-idx)
		  (vector-ref (vector-ref (last intersect-sums) r-idx) b-idx)))])
	 (if (> curr-sum max-sum) curr-sum max-sum)))])))

(define (temp-stuff)
  (when (and (= 3 l-idx) (= 1 r-idx))
	       (display curr-sum)
	       (newline)
	       (display
		(vector-ref (car side-sums) l-idx))
	       (newline)
	       (display
		(vector-ref (cadr side-sums) r-idx))
	       (newline)
	       (display
		(vector-ref (last side-sums) b-idx))
	       (newline)
	       (newline)
	       (display
		(vector-ref (vector-ref (car intersect-sums) l-idx) r-idx))
	       (newline)
	       (display
		(vector-ref (vector-ref (cadr intersect-sums) l-idx) b-idx))
	       (newline)
	       (display
		(vector-ref (vector-ref (last intersect-sums) r-idx) b-idx))
	       (newline)))
;; Now that we have these lists, we don't need the triangle!
;; To find min-sub, we find max external region!
;; Pattern matching would be great here!
(define-public (min-sub-triangle-sum-b triangle)
  (let ([side-sums (make-sides triangle)]
	[intersect-sums (make-intersections triangle)])
    ;; Using -1 since we are doing a do, while
    (let lp ([l-idx -1] [r-idx -1] [b-idx 0]
	     [intersections (init-intersections triangle)]
	     [curr-sum curr-sum] [min-sum curr-sum])
      (cond
       [(>= l-idx row-length) min-sum]
       [(>= r-idx row-length)
	(update-intersections! triangle intersections)
	(lp (1+ l-idx) -1 0 intersections curr-sum min-sum)]
       [(>= b-idx row-length)
	(update-intersections! triangle intersections)
	(lp l-idx (1+ r-idx) 0 intersections curr-sum min-sum)]
       [else
	(lp l-idx
	    r-idx (1+ b-idx)
	    intersections
	    curr-sum
	    min-sum)]))))

(define-public (make-triangle values rows)
  (let ([triangle (make-vector rows 0)])
    (let lp ([curr-row 1] [values values])
      (if (> curr-row rows) triangle
	  (begin
	    (vector-set! triangle
			 (1- curr-row)
			 (list->vector (take values curr-row)))
	    (lp (1+ curr-row) (drop values curr-row)))))))
;;; Triangles for tests

(define-public test-triangle1 (make-triangle '(1 2 3 4 5 6 1 1 1 1) 4))

(define test-triangle2 (make-triangle '(2 4 5 1 1 1) 3))

(define test-triangle3 (make-triangle '(1 2 3 4 5 6 1 1 1 1 1 1 1 1 1) 5))

(define test-triangle4 (make-triangle '(2 4 5 1 1 1 1 1 1 1) 4))

(define-public problem-triangle
  (make-triangle (linear-congruential-generator 500500) 1000))

(define-public problem-triangle-small
  (make-triangle (linear-congruential-generator 500500) 300))

(define large-triangle
  (make-triangle (iota 500500 1 -1) 400))

(define-public example-triangle
  (make-triangle 
   '(15 -14 -7 20 -13 -5 -3 8 23 -26 1 -4 -5 -18 5 -16 31 2 9 28 3)
   6))

;;; Old stuff:

;; What takes a long time: creating the sub-triangles
;; summing the whole triangle

;; Slow for large triangles
(define-public (triangle-sum triangle)
  (fold (lambda (row acc)
	    (apply + acc (vector->list row)))
	  0
	  (vector->list triangle)))

(define-public (triangle-rows triangle)
  (vector-length triangle))

;; Need to add right/left sums
;; This takes 2 and 1/2 minutes to run when accessing row-vector cache
(define (sub-triangle-sums triangle vect row-idx vert-idx curr-sum)
  (let lp ([curr-row-idx (1- (triangle-rows triangle))]
	   [sub-sums (list curr-sum)])
    (if (<= curr-row-idx (1+ row-idx)) sub-sums
	(lp
	 (1- curr-row-idx)
	 (cons (- (car sub-sums)
		  (row-sum-vect vect
;triangle
;			   curr-row-idx vert-idx
;			   (- (1+ curr-row-idx) row-idx)
		   ))
	       sub-sums)))))

;; TODO: eventually turn these two into the same function
(define (right-sub-sum triangle parent-row-idx parent-vert-idx parent-sum)
  (let lp ([curr-row-idx parent-row-idx] [curr-sum parent-sum])
    (if (>= curr-row-idx (triangle-rows triangle))
	curr-sum
	(lp (1+ curr-row-idx)
	    (- curr-sum (vector-ref
			 (vector-ref triangle curr-row-idx)
			 parent-vert-idx))))))

(define (left-sub-sum triangle parent-row-idx parent-vert-idx parent-sum)
  (let lp ([curr-row-idx parent-row-idx]
	   [curr-val-idx parent-vert-idx] [curr-sum parent-sum])
    (if (>= curr-row-idx (triangle-rows triangle))
	curr-sum
	(lp (1+ curr-row-idx)
	    (1+ curr-val-idx)
	    (- curr-sum (vector-ref
			 (vector-ref triangle curr-row-idx)
			 curr-val-idx))))))

(define hash-t
  (let ([hash-tab (make-hash-table 1000)])
    (do [(i 0 (1+ i))]
	[(> i (expt 10 6)) hash-tab]
      (hashv-set! hash-tab (list i i) i))))

(define counter 0)
(define limit (expt 10 3))

(define (row-sum-fast)
  (set! counter (if (>= counter limit) 0 (1+ counter)))
  (hashq-ref hash-t counter))

(define (row-sum-vect vect)
  (set! counter (if (>= counter limit) 0 (1+ counter)))
  (vector-ref (vector-ref vect counter) counter))

(define (row-sum-fastest) 0)

;; I wonder if this is what is taking so long...
(define (row-sum triangle row-idx val-idx row-size)
  (apply + 
	 (take (drop (vector->list (vector-ref triangle row-idx))
		     val-idx)
	       row-size)))

;; TODO: figure out the most elegant way of traversing the triangle
;; Okay need to figure out the case when vert-idx = row-idx
;; For now, doing case where we recur down right off the bat
(define-public (min-sum-sub-triangle triangle)
  (define vect
    (vector-map (lambda (val index)
		  (make-vector index index))
		(make-vector (expt 10 4) 0)
		(list->vector (iota (expt 10 4) 1))))
  (let ([curr-sum (triangle-sum triangle)])
    (let lp ([row-idx 0]
	     [vert-idx 0] [curr-sum curr-sum]
	     [min-sum curr-sum])
      ;; Need to get the proper min sum here...
      ;; Note: i am not being tal
      (cond
       [(<= (- (triangle-rows triangle) row-idx) 1) min-sum]
       [(= vert-idx row-idx)
	;; lp to the left then right
	(let ([left-min-sum
	       (lp (1+ row-idx) vert-idx
		   (left-sub-sum triangle row-idx vert-idx curr-sum)
		   (apply min min-sum
			  (left-sub-sums triangle vect row-idx vert-idx curr-sum)))])
	  ;; Now looping to the right
	  (lp (1+ row-idx) (1+ vert-idx)
	      (right-sub-sum triangle row-idx vert-idx curr-sum)
	      (apply min left-min-sum
		     (right-sub-sums triangle vect row-idx vert-idx curr-sum))))]
       [else
	(lp (1+ row-idx)
	    vert-idx (left-sub-sum triangle row-idx vert-idx curr-sum)
	    (apply min min-sum
		   (left-sub-sums triangle vect row-idx vert-idx curr-sum)))]))))

(define (left-sub-sums triangle vect parent-row-idx parent-vert-idx parent-sum)
  (sub-triangle-sums triangle
		     vect (1+ parent-row-idx) parent-vert-idx
		     (left-sub-sum triangle
				   parent-row-idx parent-vert-idx
				   parent-sum)))

(define (right-sub-sums triangle vect row-idx vert-idx curr-sum)
  (sub-triangle-sums triangle vect (1+ row-idx) (1+ vert-idx)
		     (right-sub-sum triangle row-idx vert-idx curr-sum)))

;;; Not useful for large triangles
(define (make-sub-triangle triangle row-idx val-idx)
  (let ([sub-triangle (make-vector
		       (- (vector-length triangle) row-idx)
		       #f)])
    (do [(sub-row-idx 0 (1+ sub-row-idx))]
	[(>= (+ row-idx sub-row-idx) (triangle-rows triangle)) sub-triangle]
      (let ([sub-row (make-vector (1+ sub-row-idx) #f)])
	(vector-move-left! (vector-ref triangle (+ sub-row-idx row-idx))
			   val-idx (+ 1 val-idx sub-row-idx) sub-row
			   0)
	(vector-set! sub-triangle sub-row-idx sub-row)))))

;; Would be intersted to figure out a better way to generate

;; Mini test suite
(define (test-sub-triangle-sums triangle)
  (let ([sum (triangle-sum triangle)])
    (assert (= (left-sub-sum triangle 0 0 sum)
	       (triangle-sum (make-sub-triangle triangle 1 0))))
    (assert (= (right-sub-sum triangle 0 0 sum)
	       (triangle-sum (make-sub-triangle triangle 1 1))))
    (display (right-sub-sums triangle 0 0 sum))
    (display (sub-triangle-sums triangle 1 1 (right-sub-sum triangle 0 0 sum)))
    (assert (equal? (left-sub-sums triangle 0 0 sum)
	       (sub-triangle-sums triangle 1 0 (left-sub-sum triangle 0 0 sum))))))


;; Note to self: I should have checked how long the sum would take before I implement my solution, since it is all useless now...
