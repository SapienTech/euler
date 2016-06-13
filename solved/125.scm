;; Palindromic sums
(define module (test test))

(use-modules (euler utils)
	     (srfi srfi-1)
	     (rnrs sorting))

(define (get-consecutive-square-sums n)
  (define squares (get-squares n))

  (define (init-square-sum i)
    (+ (array-ref squares i)
       (array-ref squares (1+ i))))

  ;; TODO: make current sum the thing to check..
  ;; ASSUMING we have an array with more than one item
  (let generator ((i 0) (j 1) (curr-sum (init-square-sum 0)) (acc '()))
      (cond
       ((>= i (- (array-length squares) 2)) acc)
       ((>= curr-sum n) (generator (1+ i) (+ 2 i) (init-square-sum (1+ i)) acc))
       ((>= j (array-length squares)) (generator (1+ i) (+ 2 i) (init-square-sum (1+ i)) acc))
       (else (generator i (1+ j)
			(+ curr-sum (array-ref squares (1+ j)))
			(cons curr-sum acc))))))


(define (get-squares n)
  (let generator ((i 1) (acc '()))
    (let ((square (expt i 2)))
      (if (>= square n) (list->array 1 (reverse acc))
	  (generator (1+ i) (cons square acc))))))


(define (quick-delete-duplicates lst)
  (let loop ((sorted-lst (list-sort < lst)) (unique-lst '()))
       (cond
	((= 1 (length sorted-lst)) (cons (car sorted-lst) unique-lst))
	((= (car sorted-lst) (cadr sorted-lst))
	 (loop (cdr sorted-lst) unique-lst))
	(else (loop (cdr sorted-lst) (cons (car sorted-lst) unique-lst))))))

(define-public (digit-length n)
  (string-length (number->string n)))

(display
 (fold + 0
       (filter (lambda (n)
		 (palendromic? (number->string n)))
	       (delete-duplicates (get-consecutive-square-sums (expt 10 8))))))
(newline)
