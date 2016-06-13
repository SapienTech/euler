;; Path sum: two ways

(define (make-min-sum-array array)
  ())
(define (init-min-path-array array)
  (let ((min-path-array (make-array #f
				    (array-length array)
				    (array-length array))))
    (do ((i 0 (1+ i)))
	((>= i (array-length array)))
      (array-set! min-path-array (array-ref array i 0) i 0))
    min-path-array))

(define (print-array array)
  (for-each (lambda (row)
	      (display row) (newline))
	    (array->list array)))

(define default-array
  (list->array 2
	       (list
		(list 131 673 234 103 18)
		(list 201 96 342 965 150)
		(list 630 803 746 422 111)
		(list 537 699 497 121 956)
		(list 805 732 524 37 331))))
(print-array (init-min-path-array default-array))
