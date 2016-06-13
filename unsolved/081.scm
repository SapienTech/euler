;; Path sum: two ways

(use-modules (rnrs io ports))

;; TODO: rename array
(define (make-min-sum-matrix array)
  (define min-path-array (init-min-path-array array))
  (define (rank-loop curr-rank)
    (if (>= curr-rank (array-length array)) min-path-array
	(begin
	  (update-array curr-rank)
	  (rank-loop (1+ curr-rank)))))

  ;; using sub-arrays to make indexing easier
  ;; TODO: change update-array name
  (define (update-array curr-rank)
    (let ((sub-array (make-shared-array min-path-array
					(lambda (i j)
					  (list (+ curr-rank i) (+ curr-rank j)))
					(- (array-length min-path-array) curr-rank)
					(- (array-length min-path-array) curr-rank))))
      (do ((i 1 (1+ i)))
	  ((>= i (array-length sub-array)))
	;; could refactor this (using transpose of array) to not repeat myself
	(array-set! sub-array
		    (+ (array-ref array (1+ curr-rank) (+ i curr-rank))
		       (find-min-val sub-array i))
		    1 i)
	(array-set! (transpose-array sub-array 1 0)
		    (+ (array-ref array (+ i curr-rank) (1+ curr-rank))
		       (find-min-val (transpose-array sub-array 1 0) i))
		    1 i))))
  (begin
    (rank-loop 0))
    min-path-array)

(define (find-min-val array i)
  (min (array-ref array 0 i)
       (array-ref array 1 (1- i))))

(define (init-min-path-array array)
  (let ((min-path-array (make-array #f
				    (array-length array)
				    (array-length array))))
    (array-set! min-path-array (array-ref array 0 0) 0 0)
    ;; could also refactor this
    (do ((i 1 (1+ i)))
	((>= i (array-length min-path-array)))
      (array-set! min-path-array
		  (+ (array-ref array 0 i)
		     (array-ref min-path-array 0 (1- i)))
		  0 i)
      (array-set! min-path-array
		  (+ (array-ref array i 0)
		     (array-ref min-path-array (1- i) 0))
		  i 0))
    min-path-array))

(define (print-array array)
  (for-each (lambda (row)
	      (display row) (newline))
	    (array->list array)))

(define (make-matrix-from-file file)
  (let ((port (open-input-file file)))
    (let loop ((line (get-line port)) (lst '()))
      (if (eof-object? line) (list->array 2 (reverse lst))
	  (loop (get-line port) (cons (line->number-list line) lst))))))

(define (line->number-list line)
  (map string->number (string-split line #\,)))

;; for testing purposes
(define default-array
  (list->array 2
	       (list
		(list 131 673 234 103 18)
		(list 201 96 342 965 150)
		(list 630 803 746 422 111)
		(list 537 699 497 121 956)
		(list 805 732 524 37 331))))

(let* ((min-array (make-min-sum-matrix (make-matrix-from-file "../input/p081.txt")))
       (last-index (1- (array-length min-array))))
  (display (array-ref min-array
		      last-index
		      last-index))) 
