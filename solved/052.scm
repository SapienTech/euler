;; Permuted multiples

(use-modules (srfi srfi-1))

(define-public (min-permuted-multiple start multiples)
  (let loop ((i start))
    (if (truth-fold (lambda (multiple)
		      (permuted-multiple? i multiple))
		    multiples)
	i
	(loop (1+ i)))))

(define (truth-fold proc list)
  (fold (lambda (item truth)
	  (and (proc item) truth))
	#t
	list))

(define (permuted-multiple? x multiple)
  (let* ((candidate (* x multiple))
	(l-x (string->list (number->string x)))
	(l-candidate (string->list (number->string candidate))))
    (and (= (length l-x) (length l-candidate))
	 (lset= eqv? l-x l-candidate))))
