(define-module (euler input))

(define-public default-array
  (list->array 2
	       (list
		(list 131 673 234 103 18)
		(list 201 96 342 965 150)
		(list 630 803 746 422 111)
		(list 537 699 497 121 956)
		(list 805 732 524 37 331))))

(define-public (make-matrix-from-file file)
  (let ((port (open-input-file file)))
    (let loop ((line (get-line port)) (lst '()))
      (if (eof-object? line) (list->array 2 (reverse lst))
	  (loop (get-line port) (cons (line->number-list line) lst))))))

(define-public (line->number-list line)
  (map string->number (string-split line #\,)))

;; TODO: eventually move this to io module
(define-public (print-array array)
  (for-each (lambda (row)
	      (display row) (newline))
	    (array->list array)))
