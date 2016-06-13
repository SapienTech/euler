;; Convergents of e

(use-modules (srfi srfi-1)
	     (euler utils)
	     (euler generators))

(define (num-digits-sum-for-nth-convergent convergents frac-generator)
  (define (nth-convergent)
    (let loop ((i (1- convergents)) (curr-frac (frac-generator)))
      (if (zero? i) curr-frac 
	  (loop (1- i) (frac-generator)))))
  (fold + 0
	  (number->digits
	   (numerator (nth-convergent)))))

(define (make-convergents-of-e-generator)
  (let ((frac-generator (make-convergent-frac-generator))
	(input-lst (list 1 2 1))
	(initialized? #f))
    (lambda ()
      (if initialized?
	  (let ((term (frac-generator (car input-lst))))
	    (set! input-lst
	      (append (cdr input-lst)
		      (list
		       (if (> (car input-lst) 1)
			   (+ 2 (car input-lst))
			   (car input-lst)))))
	    term)
	  (begin
	    (set! initialized? #t)
	    (frac-generator 2))))))
